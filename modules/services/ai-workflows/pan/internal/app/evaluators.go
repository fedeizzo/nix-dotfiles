package app

import (
	"context"

	lm "github.com/Cidan/lunchmoney-go"
	"pan/internal/scheduler"
)

func buildEvaluators(svcs *Services) *scheduler.EvaluatorRegistry {
	evalRegistry := scheduler.NewEvaluatorRegistry()

	evalRegistry.Register("fastmail:has_unread", func(ctx context.Context) (bool, error) {
		tags, err := svcs.Fastmail.GetTags(ctx)
		if err != nil {
			return false, err
		}
		for _, tag := range tags {
			if tag.Name == "Inbox" && tag.Unread > 0 {
				return true, nil
			}
		}
		return false, nil
	})

	evalRegistry.Register("lunchmoney:has_unreviewed", func(ctx context.Context) (bool, error) {
		st := lm.TransactionStatus("unreviewed")
		limit := int32(1)
		resp, err := svcs.LunchMoney.Transactions.List(ctx, &lm.ListTransactionsParams{
			Status: &st,
			Limit:  &limit,
		})
		if err != nil {
			return false, err
		}
		return len(resp.Transactions) > 0, nil
	})

	evalRegistry.Register("fusion:has_unread", func(ctx context.Context) (bool, error) {
		feeds, err := svcs.Fusion.GetUnreadFeeds(ctx, 1)
		if err != nil {
			return false, err
		}
		return len(feeds) > 0, nil
	})

	return evalRegistry
}
