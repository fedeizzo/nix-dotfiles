package matrix

import (
	"context"
	"encoding/json"
	"fmt"
	"iter"
	"log/slog"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"pan/internal/config"

	_ "github.com/mattn/go-sqlite3"
	"github.com/samber/oops"
	"google.golang.org/adk/agent"
	"google.golang.org/adk/runner"
	"google.golang.org/adk/session"
	"google.golang.org/adk/tool/toolconfirmation"
	"google.golang.org/genai"
	"maunium.net/go/mautrix"
	"maunium.net/go/mautrix/crypto/cryptohelper"
	"maunium.net/go/mautrix/event"
	"maunium.net/go/mautrix/format"
	"maunium.net/go/mautrix/id"
)

var (
	pendingConfirmations      = make(map[string]string) // sessionID -> toolCallID
	pendingConfirmationsMutex sync.Mutex
)

type matrixBot struct {
	client           *mautrix.Client
	r                *runner.Runner
	allowedUser      id.UserID
	allowedRoom      id.RoomID
	startTime        int64
	messageRetention time.Duration
}

func New(cfg *config.Config, r *runner.Runner) (*matrixBot, error) {
	homeserverURL := cfg.Matrix.Homeserver
	userID := cfg.Matrix.User
	password := cfg.Matrix.Password
	allowedUser := cfg.Matrix.AllowedUser
	allowedRoom := cfg.Matrix.AllowedRoom
	dataDir := cfg.Matrix.DataDir

	if homeserverURL == "" || userID == "" || password == "" {
		slog.Warn("Matrix configuration missing, skipping Matrix bot")
		return nil, nil
	}

	if err := os.MkdirAll(dataDir, 0700); err != nil {
		return nil, oops.In("bot").Wrapf(err, "failed to create data dir")
	}

	var accessToken string
	var deviceID string

	tokenPath := filepath.Join(dataDir, "access-token")
	devicePath := filepath.Join(dataDir, "device-id")

	if b, err := os.ReadFile(tokenPath); err == nil {
		accessToken = strings.TrimSpace(string(b))
	}
	if b, err := os.ReadFile(devicePath); err == nil {
		deviceID = strings.TrimSpace(string(b))
	}

	client, err := mautrix.NewClient(homeserverURL, id.UserID(userID), accessToken)
	if err != nil {
		return nil, err
	}
	if deviceID != "" {
		client.DeviceID = id.DeviceID(deviceID)
	}

	if accessToken == "" {
		// No saved token, use the password to log in and save credentials
		client, err = authenticate(client, password, userID, dataDir)
		if err != nil {
			return nil, err
		}
	}

	client, err = initCryptography(client, dataDir)
	if err != nil {
		return nil, err
	}

	bot := &matrixBot{
		client:           client,
		r:                r,
		allowedUser:      id.UserID(allowedUser),
		allowedRoom:      id.RoomID(allowedRoom),
		startTime:        time.Now().UnixMilli(),
		messageRetention: cfg.Matrix.MessageRetention,
	}

	syncer := client.Syncer.(*mautrix.DefaultSyncer)
	syncer.OnEventType(event.EventMessage, bot.handleMessage)
	syncer.OnEventType(event.StateMember, bot.handleInvite)
	syncer.OnEventType(event.EventEncrypted, func(source context.Context, evt *event.Event) {
		slog.Debug("Raw encrypted event received by syncer")
	})

	return bot, nil
}

func (b *matrixBot) Start() error {
	slog.Info("Starting Matrix bot loop", "user", b.client.UserID)

	if b.messageRetention > 0 {
		go b.runMessageRetentionJob()
	}

	return b.client.Sync()
}

func (b *matrixBot) runMessageRetentionJob() {
	// Initial cleanup on startup
	b.cleanupOldMessages()

	ticker := time.NewTicker(24 * time.Hour)
	for range ticker.C {
		b.cleanupOldMessages()
	}
}

func (b *matrixBot) cleanupOldMessages() {
	ctx := context.Background()

	var rooms []id.RoomID
	if b.allowedRoom != "" {
		rooms = append(rooms, b.allowedRoom)
	} else {
		resp, err := b.client.JoinedRooms(ctx)
		if err == nil && resp != nil {
			rooms = resp.JoinedRooms
		} else {
			slog.Error("Failed to fetch joined rooms for retention job", "error", err)
			return
		}
	}

	for _, roomID := range rooms {
		b.cleanupRoomMessages(ctx, roomID)
	}
}

func (b *matrixBot) cleanupRoomMessages(ctx context.Context, roomID id.RoomID) {
	slog.Info("Running message retention cleanup", "room", roomID)
	cutoff := time.Now().Add(-b.messageRetention).UnixMilli()

	var fromToken string
	consecutiveRedacted := 0

	for {
		resp, err := b.client.Messages(ctx, roomID, fromToken, "", 'b', nil, 100)
		if err != nil {
			slog.Error("Failed to fetch messages for cleanup", "room", roomID, "error", err)
			break
		}

		if len(resp.Chunk) == 0 {
			break
		}

		for _, evt := range resp.Chunk {
			if evt.Type == event.EventRedaction {
				continue
			}

			// We only want to redact message events, not state events (like joins, name changes, etc)
			if evt.Type != event.EventMessage && evt.Type != event.EventEncrypted {
				continue
			}

			if evt.Timestamp < cutoff {
				// Is it already redacted?
				if evt.Unsigned.RedactedBecause != nil {
					consecutiveRedacted++
					if consecutiveRedacted >= 50 {
						slog.Info("Reached previously redacted messages, stopping cleanup", "room", roomID)
						return
					}
				} else {
					consecutiveRedacted = 0
					_, err := b.client.RedactEvent(ctx, roomID, evt.ID, mautrix.ReqRedact{
						Reason: "Configured message retention policy",
					})

					// Handle Rate Limiting
					if err != nil && (strings.Contains(err.Error(), "M_LIMIT_EXCEEDED") || strings.Contains(err.Error(), "429")) {
						slog.Warn("Rate limited by homeserver, backing off for 60 seconds", "event_id", evt.ID)
						time.Sleep(60 * time.Second)

						// Retry once after backoff
						_, err = b.client.RedactEvent(ctx, roomID, evt.ID, mautrix.ReqRedact{
							Reason: "Configured message retention policy",
						})
					}

					if err != nil {
						slog.Warn("Failed to redact old message", "event_id", evt.ID, "error", err)
					} else {
						slog.Debug("Redacted old message", "event_id", evt.ID)
					}

					// Sleep 10 seconds between redactions to be gentler on the homeserver
					time.Sleep(10 * time.Second)
				}
			} else {
				consecutiveRedacted = 0
			}
		}

		if resp.End == fromToken || resp.End == "" {
			break
		}
		fromToken = resp.End
	}
	slog.Info("Finished message retention cleanup", "room", roomID)
}

func (b *matrixBot) handleInvite(source context.Context, evt *event.Event) {
	if evt.StateKey == nil || *evt.StateKey != string(b.client.UserID) {
		return
	}

	content := evt.Content.AsMember()
	if content == nil || content.Membership != event.MembershipInvite {
		return
	}

	if b.allowedUser != "" && evt.Sender != b.allowedUser {
		slog.Warn("Ignoring invite from unauthorized user", "sender", evt.Sender)
		return
	}

	slog.Info("Accepting room invite")
	if _, err := b.client.JoinRoomByID(source, evt.RoomID); err != nil {
		slog.Error("Failed to join room", "room", evt.RoomID, "error", err)
	}
}

func (b *matrixBot) shouldIgnoreMessage(evt *event.Event, content *event.MessageEventContent) bool {
	if evt.Timestamp < b.startTime {
		slog.Debug("Ignoring old message", "sender", evt.Sender)
		return true
	}
	if evt.Sender == b.client.UserID {
		return true
	}
	if b.allowedRoom != "" && evt.RoomID != b.allowedRoom {
		return true
	}
	if b.allowedUser != "" && evt.Sender != b.allowedUser {
		return true
	}
	if content == nil || content.Body == "" {
		return true
	}
	return false
}

func getSessionID(evt *event.Event, content *event.MessageEventContent) string {
	sessionID := string(evt.ID)
	if rel := content.RelatesTo; rel != nil {
		if rel.Type == event.RelThread {
			sessionID = string(rel.EventID)
		} else if rel.InReplyTo != nil {
			sessionID = string(rel.InReplyTo.EventID)
		}
	}
	return sessionID
}

func checkPendingConfirmation(sessionID string) (string, bool) {
	pendingConfirmationsMutex.Lock()
	defer pendingConfirmationsMutex.Unlock()

	funcCallID, hasConf := pendingConfirmations[sessionID]
	if hasConf {
		delete(pendingConfirmations, sessionID)
	}
	return funcCallID, hasConf
}

func buildInputMessage(content *event.MessageEventContent, funcCallID string, hasConf bool) *genai.Content {
	if !hasConf {
		return &genai.Content{
			Role:  "user",
			Parts: []*genai.Part{{Text: content.Body}},
		}
	}

	lowerBody := strings.ToLower(strings.TrimSpace(content.Body))
	approved := !(lowerBody == "no" || strings.HasPrefix(lowerBody, "no ") || strings.HasPrefix(lowerBody, "no,"))

	return &genai.Content{
		Role: "user",
		Parts: []*genai.Part{
			{
				FunctionResponse: &genai.FunctionResponse{
					ID:   funcCallID,
					Name: "adk_request_confirmation",
					Response: map[string]any{
						"confirmed": true,
						"payload": map[string]any{
							"approved": approved,
							"feedback": content.Body,
						},
					},
				},
			},
		},
	}
}

func (b *matrixBot) handleMessage(source context.Context, evt *event.Event) {
	content := evt.Content.AsMessage()
	if b.shouldIgnoreMessage(evt, content) {
		return
	}

	sessionID := getSessionID(evt, content)
	slog.Info("Matrix received message", "sessionID", sessionID)

	ctx := context.Background()
	b.reactToEvent(ctx, evt.RoomID, evt.ID, "👀")

	funcCallID, hasConf := checkPendingConfirmation(sessionID)
	msg := buildInputMessage(content, funcCallID, hasConf)

	runCtx := context.WithValue(ctx, "matrix_thread_id", sessionID)
	stream := b.r.Run(runCtx, "user", sessionID, msg, agent.RunConfig{})

	b.processAgentStream(runCtx, evt.RoomID, evt.ID, sessionID, stream)
}

func (b *matrixBot) BroadcastStream(ctx context.Context, roomID id.RoomID, eventID id.EventID, sessionID string, stream iter.Seq2[*session.Event, error]) {
	b.processAgentStream(ctx, roomID, eventID, sessionID, stream)
}

func (b *matrixBot) StartJobSession(ctx context.Context, roomID id.RoomID, message string) (id.EventID, error) {
	content := format.RenderMarkdown(message, true, false)
	content.MsgType = event.MsgNotice

	resp, err := b.client.SendMessageEvent(ctx, roomID, event.EventMessage, &content)
	if err != nil {
		return "", err
	}
	return resp.EventID, nil
}

func (b *matrixBot) processAgentStream(ctx context.Context, roomID id.RoomID, eventID id.EventID, sessionID string, stream iter.Seq2[*session.Event, error]) {
	var textBuilder strings.Builder

	for ev, err := range stream {
		if err != nil {
			slog.Error("Runner error processing stream", "error", err)
			continue
		}

		if ev.Author == "user" || ev.Content == nil {
			continue
		}

		for _, part := range ev.Content.Parts {
			if part.FunctionCall != nil {
				if part.FunctionCall.Name == "adk_request_confirmation" {
					b.handleToolConfirmationRequest(sessionID, part.FunctionCall, &textBuilder)
				} else {
					slog.Info("Agent called tool", "tool", part.FunctionCall.Name)
				}
			}
			if part.Text != "" {
				if part.Thought {
					// We can ignore thoughts or format them nicely. For now, we omit them from the final matrix reply or add them contextually.
					// Since users usually don't want to see thoughts in matrix, we'll skip them or format them as italics.
					// Let's omit them to keep the chat clean.
				} else {
					textBuilder.WriteString(part.Text)
				}
			}
		}

		// When the turn is fully complete, send the accumulated response to Matrix
		if ev.IsFinalResponse() {
			respText := textBuilder.String()
			if respText != "" {
				b.sendMatrixReply(ctx, roomID, sessionID, respText)
				textBuilder.Reset() // Reset for any subsequent turns in the same stream (though unlikely)
			}
			b.reactToEvent(ctx, roomID, eventID, "✅")
		}
	}
}

func (b *matrixBot) handleToolConfirmationRequest(sessionID string, funcCall *genai.FunctionCall, textBuilder *strings.Builder) {
	pendingConfirmationsMutex.Lock()
	pendingConfirmations[sessionID] = funcCall.ID
	pendingConfirmationsMutex.Unlock()

	toolName := "an action"
	if origCall := funcCall.Args["originalFunctionCall"]; origCall != nil {
		// Attempt direct map assertion
		if m, ok := origCall.(map[string]any); ok {
			if name, ok := m["name"].(string); ok {
				toolName = fmt.Sprintf("'%s'", name)
			}
		} else {
			// Fallback: marshal and unmarshal to extract 'name' robustly
			b, err := json.Marshal(origCall)
			if err == nil {
				var extracted struct {
					Name string `json:"name"`
				}
				if json.Unmarshal(b, &extracted) == nil && extracted.Name != "" {
					toolName = fmt.Sprintf("'%s'", extracted.Name)
				}
			}
		}
	}

	hint := "Please confirm the action."
	if tc, ok := funcCall.Args["toolConfirmation"].(*toolconfirmation.ToolConfirmation); ok {
		hint = tc.Hint
	} else if tcMap, ok := funcCall.Args["toolConfirmation"].(map[string]any); ok {
		if h, ok := tcMap["hint"].(string); ok {
			hint = h
		}
	}

	if textBuilder.Len() > 0 {
		textBuilder.WriteString("\n\n")
	}
	textBuilder.WriteString(fmt.Sprintf("⚠️ **Action Required: %s**\n%s\n*(Reply with 'yes' to approve or 'no' to reject)*", toolName, hint))
}

func (b *matrixBot) reactToEvent(ctx context.Context, roomID id.RoomID, eventID id.EventID, emoji string) {
	if eventID == "" || !strings.HasPrefix(string(eventID), "$") {
		return
	}

	_, err := b.client.SendMessageEvent(ctx, roomID, event.EventReaction, &event.ReactionEventContent{
		RelatesTo: event.RelatesTo{
			Type:    event.RelAnnotation,
			EventID: eventID,
			Key:     emoji,
		},
	})
	if err != nil {
		slog.Error("Failed to send reaction", "error", err)
	}
}

func (b *matrixBot) sendMatrixReply(ctx context.Context, roomID id.RoomID, sessionID string, text string) {
	var relatesTo *event.RelatesTo
	if strings.HasPrefix(sessionID, "$") {
		relatesTo = &event.RelatesTo{
			Type:    event.RelThread,
			EventID: id.EventID(sessionID),
		}
	}

	content := format.RenderMarkdown(text, true, false)
	content.RelatesTo = relatesTo

	_, err := b.client.SendMessageEvent(ctx, roomID, event.EventMessage, &content)
	if err != nil {
		slog.Error("Failed to send matrix message", "error", err)
	}
}

// authenticate performs login and saves credentials to dataDir
func authenticate(client *mautrix.Client, password, userID, dataDir string) (*mautrix.Client, error) {
	if password != "" {
		resp, err := client.Login(context.Background(), &mautrix.ReqLogin{
			Type: mautrix.AuthTypePassword,
			Identifier: mautrix.UserIdentifier{
				Type: mautrix.IdentifierTypeUser,
				User: userID,
			},
			Password:                 password,
			InitialDeviceDisplayName: "Pan Intelligent Agent",
			StoreCredentials:         true,
		})
		if err != nil {
			return nil, oops.In("bot").Wrapf(err, "matrix login failed")
		}

		tokenPath := filepath.Join(dataDir, "access-token")
		devicePath := filepath.Join(dataDir, "device-id")

		_ = os.WriteFile(tokenPath, []byte(resp.AccessToken), 0600)
		_ = os.WriteFile(devicePath, []byte(resp.DeviceID), 0600)

		slog.Info("Logged into Matrix and saved credentials to dataDir", "device_id", resp.DeviceID)
	} else {
		return nil, oops.In("bot").Errorf("no password provided and no saved token found")
	}
	return client, nil
}

func initCryptography(client *mautrix.Client, dataDir string) (*mautrix.Client, error) {
	if dataDir == "" {
		dataDir = "."
	}
	if err := os.MkdirAll(dataDir, 0700); err != nil {
		return nil, oops.In("bot").Wrapf(err, "failed to create MATRIX_DATA_DIR")
	}

	dbPath := filepath.Join(dataDir, "matrix-crypto.db")

	// The helper reads client.DeviceID and client.AccessToken perfectly here
	helper, err := cryptohelper.NewCryptoHelper(client, []byte("pan-agent-crypto-key"), dbPath)
	if err != nil {
		return nil, oops.In("bot").Wrapf(err, "failed to init cryptohelper")
	}

	if err = helper.Init(context.Background()); err != nil {
		return nil, oops.In("bot").Wrapf(err, "failed to init crypto")
	}
	client.Crypto = helper

	return client, nil
}
