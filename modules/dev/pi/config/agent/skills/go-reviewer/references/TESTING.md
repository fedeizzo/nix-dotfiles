# Go Testing Conventions

## Test File Organization

```
internal/
├── api/
│   ├── api.go
│   └── api_test.go          # Colocated
├── repositories/
│   ├── calls.go
│   └── calls_test.go        # Colocated
└── service/
    ├── service.go
    └── service_test.go      # Colocated
```

## Libraries

| Library         | Purpose                | Import                                        |
| --------------- | ---------------------- | --------------------------------------------- |
| testify/assert  | Assertions             | `github.com/stretchr/testify/assert`          |
| testify/require | Fail-fast assertions   | `github.com/stretchr/testify/require`         |
| testify/mock    | Mock expectations      | `github.com/stretchr/testify/mock`            |
| gotest.tools    | Alternative assertions | `gotest.tools/assert`                         |
| testcontainers  | Docker integration     | `github.com/testcontainers/testcontainers-go` |
| mockgen         | Auto-generate mocks    | `github.com/golang/mock/mockgen`              |

## Table-Driven Tests

### Standard Pattern

```go
func TestParseUserID(t *testing.T) {
    tests := []struct {
        name    string
        input   string
        want    int64
        wantErr bool
    }{
        {
            name:  "valid numeric",
            input: "12345",
            want:  12345,
        },
        {
            name:    "invalid string",
            input:   "abc",
            wantErr: true,
        },
        {
            name:  "empty returns zero",
            input: "",
            want:  0,
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            got, err := ParseUserID(tt.input)

            if tt.wantErr {
                assert.Error(t, err)
                return
            }

            assert.NoError(t, err)
            assert.Equal(t, tt.want, got)
        })
    }
}
```

### With Complex Setup

```go
func TestConferenceController(t *testing.T) {
    type fields struct {
        maxCalls   int
        redundancy int
    }
    type args struct {
        conferenceID string
        nodeCount    int
    }

    tests := []struct {
        name   string
        fields fields
        args   args
        want   []string
    }{
        {
            name: "selects two nodes for redundancy",
            fields: fields{
                maxCalls:   100,
                redundancy: 2,
            },
            args: args{
                conferenceID: "conf-123",
                nodeCount:    5,
            },
            want: []string{"node-1", "node-2"},
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            ctrl := NewController(tt.fields.maxCalls, tt.fields.redundancy)
            got := ctrl.SelectNodes(tt.args.conferenceID, tt.args.nodeCount)
            assert.ElementsMatch(t, tt.want, got)
        })
    }
}
```

## Mock Patterns

### Manual Mock with Interface Compliance

```go
type mockDatalayer struct {
    channel     call.BotChannel
    channelErr  error
}

// Interface compliance check
var _ datalayer.Client = (*mockDatalayer)(nil)

func (m *mockDatalayer) GetBotChannelById(ctx context.Context, id string) (call.BotChannel, error) {
    return m.channel, m.channelErr
}

func (m *mockDatalayer) GetConversation(ctx context.Context, id string) (*datalayer.Conversation, error) {
    return nil, nil  // Not needed for this test
}
```

### testify/mock Pattern

```go
type MockRedisClient struct {
    mock.Mock
}

func (m *MockRedisClient) Get(ctx context.Context, key string) (string, error) {
    args := m.Called(ctx, key)
    return args.String(0), args.Error(1)
}

func (m *MockRedisClient) Set(ctx context.Context, key, value string) error {
    args := m.Called(ctx, key, value)
    return args.Error(0)
}

// Usage in test
func TestCacheGet(t *testing.T) {
    mockRedis := new(MockRedisClient)
    mockRedis.On("Get", mock.Anything, "user:123").Return(`{"name":"John"}`, nil)

    cache := NewCache(mockRedis)
    user, err := cache.GetUser(context.Background(), "123")

    assert.NoError(t, err)
    assert.Equal(t, "John", user.Name)
    mockRedis.AssertExpectations(t)
}
```

### Auto-Generated Mock (mockgen)

```go
// In repository.go
//go:generate mockgen -package mocks -destination mocks/repository_mock.go . Repository

type Repository interface {
    GetUser(ctx context.Context, id string) (*User, error)
    SaveUser(ctx context.Context, user *User) error
}
```

```go
// In repository_test.go
func TestService(t *testing.T) {
    ctrl := gomock.NewController(t)
    defer ctrl.Finish()

    mockRepo := mocks.NewMockRepository(ctrl)
    mockRepo.EXPECT().
        GetUser(gomock.Any(), "123").
        Return(&User{Name: "John"}, nil)

    svc := NewService(mockRepo)
    user, err := svc.GetUser(context.Background(), "123")

    assert.NoError(t, err)
    assert.Equal(t, "John", user.Name)
}
```

## Integration Tests

### Build Tag Pattern

```go
//go:build integration
// +build integration

package integration

import (
    "context"
    "testing"

    "github.com/stretchr/testify/assert"
    "github.com/testcontainers/testcontainers-go"
)

func TestClickHouseIntegration(t *testing.T) {
    ctx := context.Background()

    // Start ClickHouse container
    container, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
        ContainerRequest: testcontainers.ContainerRequest{
            Image:        "clickhouse/clickhouse-server:latest",
            ExposedPorts: []string{"9000/tcp"},
        },
        Started: true,
    })
    require.NoError(t, err)
    defer container.Terminate(ctx)

    // Get connection details
    host, _ := container.Host(ctx)
    port, _ := container.MappedPort(ctx, "9000")

    // Run tests against real ClickHouse
    client := NewClient(fmt.Sprintf("%s:%s", host, port.Port()))

    err = client.CreateTable(ctx, "test_table")
    assert.NoError(t, err)
}
```

### Skip Pattern (Alternative)

```go
//go:build skip
// +build skip

package ingestor

// Tests that require external services
func TestIngestBatch(t *testing.T) {
    // This test is skipped by default
    // Run with: go test -tags=skip ./...
}
```

## Concurrent Access Testing

```go
func TestConcurrentMapAccess(t *testing.T) {
    controller := NewController()
    concurrency := 100

    var wg sync.WaitGroup
    startSignal := make(chan struct{})

    // Spawn goroutines
    for i := 0; i < concurrency; i++ {
        wg.Add(1)
        go func(id int) {
            defer wg.Done()
            <-startSignal  // Wait for signal

            confID := fmt.Sprintf("conf-%d", id%10)
            controller.StoreConference(confID, &Conference{ID: confID})
            _ = controller.GetConference(confID)
        }(i)
    }

    // Start all goroutines simultaneously
    close(startSignal)
    wg.Wait()

    // Verify no race conditions occurred
    assert.True(t, controller.Count() <= 10)
}
```

## Common Assertions

```go
// Equality
assert.Equal(t, expected, actual)
assert.NotEqual(t, unexpected, actual)

// Nil checks
assert.Nil(t, value)
assert.NotNil(t, value)

// Error handling
assert.NoError(t, err)
assert.Error(t, err)
assert.ErrorIs(t, err, ErrNotFound)
assert.ErrorContains(t, err, "not found")

// Collections
assert.Len(t, slice, 5)
assert.Empty(t, slice)
assert.Contains(t, slice, item)
assert.ElementsMatch(t, expected, actual)  // Order-independent

// Booleans
assert.True(t, condition)
assert.False(t, condition)

// Fail-fast (require)
require.NoError(t, err)  // Stops test immediately on failure
require.NotNil(t, value)
```

## Test Helpers

```go
// tools/tools.go - Declare test dependencies
//go:build tools
// +build tools

package tools

import (
    _ "github.com/golang/mock/mockgen"
    _ "github.com/twitchtv/circuitgen"
)
```

Run mock generation:

```bash
go generate ./...
```
