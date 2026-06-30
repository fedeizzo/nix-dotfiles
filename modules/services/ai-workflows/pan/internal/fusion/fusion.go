package fusion

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"time"

	"github.com/samber/oops"
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

type Service interface {
	GetUnreadFeeds(ctx context.Context, limit int) ([]FeedItem, error)
	MarkFeedsAsRead(ctx context.Context, ids []int) error
}

var _ Service = (*client)(nil)

type client struct {
	endpoint string
	password string
	client   *http.Client
}

type Option func(*client)

func WithHTTPClient(httpClient *http.Client) Option {
	return func(c *client) {
		c.client = httpClient
	}
}

func New(endpoint, password string, opts ...Option) *client {
	// Ensure endpoint ends with /api if not already, but we assume endpoint is the base URL
	c := &client{
		endpoint: endpoint,
		password: password,
		client:   &http.Client{Timeout: 10 * time.Second},
	}
	for _, opt := range opts {
		opt(c)
	}
	return c
}

func (c *client) getSessionCookie(ctx context.Context) (*http.Cookie, error) {
	loginURL := fmt.Sprintf("%s/sessions", c.endpoint)
	reqBody, err := json.Marshal(map[string]string{"password": c.password})
	if err != nil {
		return nil, oops.In("fusion").Wrapf(err, "failed to marshal login body")
	}

	req, err := http.NewRequestWithContext(ctx, "POST", loginURL, bytes.NewReader(reqBody))
	if err != nil {
		return nil, oops.In("fusion").Wrapf(err, "failed to create login request")
	}
	req.Header.Set("Content-Type", "application/json")

	resp, err := c.client.Do(req)
	if err != nil {
		return nil, oops.In("fusion").Wrapf(err, "failed to execute login request")
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, oops.In("fusion").Errorf("failed to login: status %d", resp.StatusCode)
	}

	for _, cookie := range resp.Cookies() {
		if cookie.Name == "session" {
			return cookie, nil
		}
	}

	return nil, oops.In("fusion").Errorf("fusion: session cookie not found in response")
}

func (c *client) GetUnreadFeeds(ctx context.Context, limit int) ([]FeedItem, error) {
	cookie, err := c.getSessionCookie(ctx)
	if err != nil {
		return nil, oops.In("fusion").Wrapf(err, "auth error")
	}

	u, _ := url.Parse(fmt.Sprintf("%s/items", c.endpoint))
	q := u.Query()
	q.Set("unread", "true")
	q.Set("limit", fmt.Sprintf("%d", limit))
	u.RawQuery = q.Encode()

	req, err := http.NewRequestWithContext(ctx, "GET", u.String(), nil)
	if err != nil {
		return nil, oops.In("fusion").Wrapf(err, "failed to create get feeds request")
	}
	req.AddCookie(cookie)

	resp, err := c.client.Do(req)
	if err != nil {
		return nil, oops.In("fusion").Wrapf(err, "failed to execute get feeds request")
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, oops.In("fusion").Errorf("failed to get items: status %d", resp.StatusCode)
	}

	var res struct {
		Data  []FeedItem `json:"data"`
		Total int        `json:"total"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&res); err != nil {
		return nil, oops.In("fusion").Wrapf(err, "failed to decode get feeds response")
	}

	return res.Data, nil
}

func (c *client) MarkFeedsAsRead(ctx context.Context, ids []int) error {
	if len(ids) == 0 {
		return nil
	}

	cookie, err := c.getSessionCookie(ctx)
	if err != nil {
		return oops.In("fusion").Wrapf(err, "auth error")
	}

	markURL := fmt.Sprintf("%s/items/-/read", c.endpoint)
	reqBody, err := json.Marshal(map[string]interface{}{"ids": ids})
	if err != nil {
		return oops.In("fusion").Wrapf(err, "failed to marshal ids")
	}

	req, err := http.NewRequestWithContext(ctx, "PATCH", markURL, bytes.NewReader(reqBody))
	if err != nil {
		return oops.In("fusion").Wrapf(err, "failed to create mark feeds request")
	}
	req.Header.Set("Content-Type", "application/json")
	req.AddCookie(cookie)

	resp, err := c.client.Do(req)
	if err != nil {
		return oops.In("fusion").Wrapf(err, "failed to execute mark feeds request")
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusNoContent && resp.StatusCode != http.StatusOK {
		return oops.In("fusion").Errorf("failed to mark feeds as read: status %d", resp.StatusCode)
	}

	return nil
}
