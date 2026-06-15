package fusion

import (
	"fmt"
	"time"

	"pan/internal/events"
	fusionsvc "pan/internal/fusion"

	"github.com/dgraph-io/ristretto"
	"go.opentelemetry.io/otel"
	"google.golang.org/adk/tool"
)

type FusionToolset struct {
	service fusionsvc.FusionService
	cache   *ristretto.Cache
}

func New(service fusionsvc.FusionService) (FusionToolset, error) {
	cache, err := ristretto.NewCache(&ristretto.Config{
		NumCounters: 1e7,
		MaxCost:     1 << 30,
		BufferItems: 64,
	})
	if err != nil {
		return FusionToolset{}, fmt.Errorf("failed to initialize cache: %w", err)
	}
	return FusionToolset{service: service, cache: cache}, nil
}

type GetUnreadFeedsInput struct {
	Limit int `json:"limit" jsonschema:"Maximum number of feeds to fetch. Maximum 15."`
}

type FeedWithContext struct {
	ID              int    `json:"id"`
	FeedID          int    `json:"feed_id"`
	Title           string `json:"title"`
	Link            string `json:"link"`
	Content         string `json:"content"`
	PubDate         int64  `json:"pub_date"`
	Unread          bool   `json:"unread"`
	CreatedAt       int64  `json:"created_at"`
	UserPreferences string `json:"user_preferences" jsonschema:"User preferences for this topic. Use this to decide if the news should be skipped."`
}

type GetUnreadFeedsOutput struct {
	Feeds []FeedWithContext `json:"feeds"`
}

func (f *FusionToolset) GetUnreadFeeds(ctx tool.Context, input GetUnreadFeedsInput) (GetUnreadFeedsOutput, error) {
	ctxTr, span := otel.Tracer("pan.agent.fusion").Start(ctx, "GetUnreadFeeds")
	defer span.End()

	events.Publish(events.ToolInvokedEvent{
		ToolName:   "GetUnreadFeeds",
		Attributes: map[string]any{"component": "AgentTool"},
	})

	if input.Limit <= 0 || input.Limit > 15 {
		input.Limit = 10
	}

	feeds, err := f.service.GetUnreadFeeds(ctxTr, input.Limit)

	var result []FeedWithContext
	if err == nil {
		for _, feed := range feeds {
			pref := ""
			query := fmt.Sprintf("Does the user like this RSS feed topic? Title: %s", feed.Title)
			if memResp, err := ctx.SearchMemory(ctx, query); err == nil && memResp != nil {
				if len(memResp.Memories) > 0 {
					if txt := memResp.Memories[0].Content; txt != nil {
						pref = fmt.Sprintf("%v", txt)
					}
				}
			}
			result = append(result, FeedWithContext{
				ID:              feed.ID,
				FeedID:          feed.FeedID,
				Title:           feed.Title,
				Link:            feed.Link,
				Content:         feed.Content,
				PubDate:         feed.PubDate,
				Unread:          feed.Unread,
				CreatedAt:       feed.CreatedAt,
				UserPreferences: pref,
			})
		}
	}

	if err == nil && len(result) > 0 {
		threadID, ok := ctx.Value("matrix_thread_id").(string)
		if !ok || threadID == "" {
			return GetUnreadFeedsOutput{}, fmt.Errorf("failed to extract matrix_thread_id from context")
		}
		
		var feedIDs []int
		for _, feed := range result {
			feedIDs = append(feedIDs, feed.ID)
		}
		
		cacheKey := fmt.Sprintf("active_feeds_%s", threadID)
		f.cache.SetWithTTL(cacheKey, feedIDs, 1, 24*time.Hour)
	}

	events.Publish(events.ToolCompletedEvent{
		ToolName:   "GetUnreadFeeds",
		Success:    err == nil,
		Error:      err,
		Attributes: map[string]any{"count": len(feeds), "component": "AgentTool"},
	})

	return GetUnreadFeedsOutput{Feeds: result}, err
}

type MarkFeedsAsReadInput struct {
}

type MarkFeedsAsReadOutput struct {
	Success bool
	Message string
}

func (f *FusionToolset) MarkFeedsAsRead(ctx tool.Context, input MarkFeedsAsReadInput) (MarkFeedsAsReadOutput, error) {
	ctxTr, span := otel.Tracer("pan.agent.fusion").Start(ctx, "MarkFeedsAsRead")
	defer span.End()

	threadID, ok := ctx.Value("matrix_thread_id").(string)
	if !ok || threadID == "" {
		return MarkFeedsAsReadOutput{Success: false, Message: "failed to extract matrix_thread_id from context"}, fmt.Errorf("failed to extract matrix_thread_id from context")
	}

	cacheKey := fmt.Sprintf("active_feeds_%s", threadID)
	val, found := f.cache.Get(cacheKey)
	if !found || val == nil {
		return MarkFeedsAsReadOutput{Success: true, Message: "Feeds already marked as read or cache expired"}, nil
	}
	
	feedIDs, ok := val.([]int)
	if !ok {
		return MarkFeedsAsReadOutput{Success: false, Message: "invalid cache type"}, fmt.Errorf("invalid cache type")
	}

	events.Publish(events.ToolInvokedEvent{
		ToolName:   "MarkFeedsAsRead",
		Attributes: map[string]any{"feedCount": len(feedIDs), "component": "AgentTool"},
	})

	err := f.service.MarkFeedsAsRead(ctxTr, feedIDs)

	events.Publish(events.ToolCompletedEvent{
		ToolName:   "MarkFeedsAsRead",
		Success:    err == nil,
		Error:      err,
		Attributes: map[string]any{"feedCount": len(feedIDs), "component": "AgentTool"},
	})

	if err != nil {
		return MarkFeedsAsReadOutput{Success: false, Message: err.Error()}, err
	}

	f.cache.Del(cacheKey)

	return MarkFeedsAsReadOutput{Success: true, Message: fmt.Sprintf("Successfully marked %d feeds as read", len(feedIDs))}, nil
}
