package fusion

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
)

type FeedItem struct {
	ID        int    `json:"id"`
	FeedID    int    `json:"feed_id"`
	Title     string `json:"title"`
	Link      string `json:"link"`
	Content   string `json:"content"`
	PubDate   int64  `json:"pub_date"`
	Unread    bool   `json:"unread"`
	CreatedAt int64  `json:"created_at"`
}

type FusionService interface {
	GetUnreadFeeds(ctx context.Context, limit int) ([]FeedItem, error)
	MarkFeedsAsRead(ctx context.Context, ids []int) error
}

type fusionClient struct {
	endpoint string
	password string
	client   *http.Client
}

func NewFusionService(endpoint, password string) FusionService {
	// Ensure endpoint ends with /api if not already, but we assume endpoint is the base URL
	return &fusionClient{
		endpoint: endpoint,
		password: password,
		client:   &http.Client{},
	}
}

func (c *fusionClient) getSessionCookie(ctx context.Context) (*http.Cookie, error) {
	loginURL := fmt.Sprintf("%s/sessions", c.endpoint)
	reqBody, _ := json.Marshal(map[string]string{"password": c.password})
	
	req, err := http.NewRequestWithContext(ctx, "POST", loginURL, bytes.NewReader(reqBody))
	if err != nil {
		return nil, err
	}
	req.Header.Set("Content-Type", "application/json")

	resp, err := c.client.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("failed to login: status %d", resp.StatusCode)
	}

	for _, cookie := range resp.Cookies() {
		if cookie.Name == "session" {
			return cookie, nil
		}
	}
	
	return nil, fmt.Errorf("session cookie not found in response")
}

func (c *fusionClient) GetUnreadFeeds(ctx context.Context, limit int) ([]FeedItem, error) {
	cookie, err := c.getSessionCookie(ctx)
	if err != nil {
		return nil, fmt.Errorf("auth error: %w", err)
	}

	u, _ := url.Parse(fmt.Sprintf("%s/items", c.endpoint))
	q := u.Query()
	q.Set("unread", "true")
	q.Set("limit", fmt.Sprintf("%d", limit))
	u.RawQuery = q.Encode()

	req, err := http.NewRequestWithContext(ctx, "GET", u.String(), nil)
	if err != nil {
		return nil, err
	}
	req.AddCookie(cookie)

	resp, err := c.client.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("failed to get items: status %d", resp.StatusCode)
	}

	var res struct {
		Data  []FeedItem `json:"data"`
		Total int        `json:"total"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&res); err != nil {
		return nil, err
	}

	return res.Data, nil
}

func (c *fusionClient) MarkFeedsAsRead(ctx context.Context, ids []int) error {
	cookie, err := c.getSessionCookie(ctx)
	if err != nil {
		return fmt.Errorf("auth error: %w", err)
	}

	markURL := fmt.Sprintf("%s/items/-/read", c.endpoint)
	reqBody, _ := json.Marshal(map[string]interface{}{"ids": ids})

	req, err := http.NewRequestWithContext(ctx, "PATCH", markURL, bytes.NewReader(reqBody))
	if err != nil {
		return err
	}
	req.Header.Set("Content-Type", "application/json")
	req.AddCookie(cookie)

	resp, err := c.client.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusNoContent && resp.StatusCode != http.StatusOK {
		return fmt.Errorf("failed to mark feeds as read: status %d", resp.StatusCode)
	}

	return nil
}
