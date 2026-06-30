package fastmail

import (
	"context"
	"fmt"
	"log/slog"
	"strings"

	"git.sr.ht/~rockorager/go-jmap"
	"git.sr.ht/~rockorager/go-jmap/mail"
	"git.sr.ht/~rockorager/go-jmap/mail/email"
	"git.sr.ht/~rockorager/go-jmap/mail/mailbox"
	"github.com/samber/oops"

	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/attribute"
)

type Tag struct {
	ID     string
	Name   string
	Emails uint
	Unread uint
}

type Email struct {
	ID      string
	Subject string
	From    string
	To      string
	Preview string
	// TextBody string
}

type Service interface {
	GetTags(ctx context.Context) ([]Tag, error)
	GetUnreadEmailForTag(ctx context.Context, tag Tag) (*Email, error)
	AddTagToEmail(ctx context.Context, emailID string, tag Tag) error
	MarkEmailAsSeen(ctx context.Context, emailID string) error
}

type JMAPClient interface {
	Do(req *jmap.Request) (*jmap.Response, error)
}

type fastmail struct {
	client JMAPClient
	id     jmap.ID
}

// Compile-time interface check
var _ Service = (*fastmail)(nil)

func New(client *jmap.Client) *fastmail {
	// Get the account ID of the primary mail account
	id := client.Session.PrimaryAccounts[mail.URI]
	return &fastmail{client: client, id: id}
}

func (f *fastmail) GetTags(ctx context.Context) ([]Tag, error) {
	_, span := otel.Tracer("pan.service.fastmail").Start(ctx, "GetTags")
	defer span.End()

	req := &jmap.Request{}
	req.Invoke(&mailbox.Get{Account: f.id})

	resp, err := f.client.Do(req)
	if err != nil {
		return nil, oops.In("fastmail").Wrapf(err, "error getting tags from fastmail")
	}

	var tags []Tag

	for _, inv := range resp.Responses {
		switch r := inv.Args.(type) {
		case *mailbox.GetResponse:
			tags = make([]Tag, 0, len(r.List))
			for _, mbox := range r.List {
				tags = append(
					tags,
					Tag{
						ID:     string(mbox.ID),
						Name:   mbox.Name,
						Emails: uint(mbox.TotalEmails),
						Unread: uint(mbox.UnreadEmails),
					},
				)
			}
		default:
			return nil, oops.In("fastmail").Errorf("error while casting response from get tags")
		}
	}

	return tags, nil
}

func (f *fastmail) GetUnreadEmailForTag(ctx context.Context, tag Tag) (*Email, error) {
	ctxTr, span := otel.Tracer("pan.service.fastmail").Start(ctx, "GetUnreadEmailForTag")
	span.SetAttributes(attribute.String("tagName", tag.Name), attribute.String("tagID", tag.ID))
	defer span.End()

	slog.Info("Querying unread email for tag", "tagName", tag.Name, "tagID", tag.ID, "component", "Fastmail")
	req := &jmap.Request{}
	req.Invoke(&email.Query{
		Account: f.id,
		Filter: &email.FilterCondition{
			InMailbox:  jmap.ID(tag.ID),
			NotKeyword: "$seen",
		},
		Limit: 1,
	})

	resp, err := f.client.Do(req)
	if err != nil {
		return nil, oops.In("fastmail").Wrapf(err, "error querying emails")
	}

	emailIDs := []jmap.ID{}
	for _, inv := range resp.Responses {
		if r, ok := inv.Args.(*email.QueryResponse); ok {
			emailIDs = r.IDs
		}
	}

	if len(emailIDs) == 0 {
		slog.Info("No unread email found for tag", "tagName", tag.Name, "component", "Fastmail")
		return nil, nil // No unread email
	}

	slog.Info("Found unread email. Fetching full content...", "tagName", tag.Name, "component", "Fastmail")
	return f.getEmail(ctxTr, emailIDs[0])
}

func (f *fastmail) getEmail(ctx context.Context, id jmap.ID) (*Email, error) {
	_, span := otel.Tracer("pan.service.fastmail").Start(ctx, "getEmail")
	defer span.End()

	req := &jmap.Request{}
	req.Invoke(&email.Get{
		Account:             f.id,
		IDs:                 []jmap.ID{id},
		Properties:          []string{"id", "subject", "from", "to", "preview", "textBody", "bodyValues"},
		FetchTextBodyValues: true,
	})

	resp, err := f.client.Do(req)
	if err != nil {
		return nil, oops.In("fastmail").Wrapf(err, "error getting email")
	}

	for _, inv := range resp.Responses {
		r, ok := inv.Args.(*email.GetResponse)
		if !ok {
			continue
		}

		for _, e := range r.List {
			var fromStr string
			if len(e.From) > 0 {
				fromStr = e.From[0].String()
			}

			var toStr string
			if len(e.To) > 0 {
				tos := make([]string, 0, len(e.To))
				for _, t := range e.To {
					tos = append(tos, t.String())
				}
				toStr = strings.Join(tos, ", ")
			}

			var textBodyBuilder strings.Builder
			for _, part := range e.TextBody {
				if bv, ok := e.BodyValues[part.PartID]; ok {
					textBodyBuilder.WriteString(bv.Value)
				}
			}

			slog.Info("Successfully fetched email content", "component", "Fastmail")
			return &Email{
				ID:      string(e.ID),
				Subject: e.Subject,
				From:    fromStr,
				To:      toStr,
				Preview: e.Preview,
				// TextBody: textBodyBuilder.String(), omit text body for now to speed up development
			}, nil
		}
	}

	return nil, oops.In("fastmail").Errorf("email not found")
}

func (f *fastmail) AddTagToEmail(ctx context.Context, emailID string, tag Tag) error {
	_, span := otel.Tracer("pan.service.fastmail").Start(ctx, "AddTagToEmail")
	span.SetAttributes(attribute.String("emailID", emailID), attribute.String("tagID", tag.ID))
	defer span.End()

	slog.Info("Adding tag to email", "emailID", emailID, "tagID", tag.ID, "component", "Fastmail")

	req := &jmap.Request{}
	req.Invoke(&email.Set{
		Account: f.id,
		Update: map[jmap.ID]jmap.Patch{
			jmap.ID(emailID): {
				fmt.Sprintf("mailboxIds/%s", tag.ID): true,
			},
		},
	})

	resp, err := f.client.Do(req)
	if err != nil {
		return oops.In("fastmail").Wrapf(err, "error adding tag to email")
	}

	for _, inv := range resp.Responses {
		r, ok := inv.Args.(*email.SetResponse)
		if !ok || len(r.NotUpdated) == 0 {
			continue
		}

		setErr, ok := r.NotUpdated[jmap.ID(emailID)]
		if !ok {
			continue
		}

		desc := ""
		if setErr.Description != nil {
			desc = *setErr.Description
		}
		return oops.In("fastmail").Errorf("failed to add tag: %s - %s", setErr.Type, desc)
	}

	return nil
}

func (f *fastmail) MarkEmailAsSeen(ctx context.Context, emailID string) error {
	_, span := otel.Tracer("pan.service.fastmail").Start(ctx, "MarkEmailAsSeen")
	span.SetAttributes(attribute.String("emailID", emailID))
	defer span.End()

	slog.Info("Marking email as seen", "emailID", emailID, "component", "Fastmail")

	req := &jmap.Request{}
	req.Invoke(&email.Set{
		Account: f.id,
		Update: map[jmap.ID]jmap.Patch{
			jmap.ID(emailID): {
				"keywords/$seen": true,
			},
		},
	})

	resp, err := f.client.Do(req)
	if err != nil {
		return oops.In("fastmail").Wrapf(err, "error marking email as seen")
	}

	for _, inv := range resp.Responses {
		r, ok := inv.Args.(*email.SetResponse)
		if !ok || len(r.NotUpdated) == 0 {
			continue
		}

		setErr, ok := r.NotUpdated[jmap.ID(emailID)]
		if !ok {
			continue
		}

		desc := ""
		if setErr.Description != nil {
			desc = *setErr.Description
		}
		return oops.In("fastmail").Errorf("failed to mark email as seen: %s - %s", setErr.Type, desc)
	}

	return nil
}
