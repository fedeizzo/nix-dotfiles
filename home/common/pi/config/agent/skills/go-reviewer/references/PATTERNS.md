# Go Code Patterns Reference

## 1. Project Structure

```
services/<name>/
├── cmd/<name>/main.go       # Entry point
├── internal/
│   ├── api/                 # HTTP handlers
│   ├── config/              # Configuration
│   └── repositories/        # Data access
├── pkg/                     # Public packages
├── go.mod
└── go.sum
```

Shared libraries: `packages/common/` (errx, logger, redis, metrics, tracing, httpx)

## 2. Interface Patterns

### Define Where Used

```go
// In consuming package (repositories/calls.go)
type CallsRepository interface {
    GetCall(ctx context.Context, id string) (*Call, error)
    CreateCall(ctx context.Context, call *Call) error
}

// Implementation (same or different package)
type callsRepoImpl struct {
    redis redis.Client
}

// Compliance check
var _ CallsRepository = (*callsRepoImpl)(nil)
```

### Constructor Returns Interface

```go
func NewCallsRepository(redis redis.Client) CallsRepository {
    return &callsRepoImpl{redis: redis}
}
```

## 3. Config Pattern

```go
type Config struct {
    // Group by category
    Port    string
    Env     string

    // Redis
    RedisURL      string
    RedisPassword string

    // Kafka
    KafkaBrokers string
}

func LoadFromEnv(ctx context.Context) Config {
    _ = godotenv.Load()
    var conf Config

    conf.Port = os.Getenv("PORT")
    if conf.Port == "" {
        conf.Port = "8080"  // Default
    }

    conf.RedisURL = os.Getenv("REDIS_URL")
    if conf.RedisURL == "" {
        logger.Fatal(ctx, errx.New("REDIS_URL required"))
    }

    return conf
}
```

## 4. HTTP Handler Patterns

### Full Handler Template

```go
func (a *API) handleRequest(w http.ResponseWriter, req *http.Request) {
    // 1. Start tracing
    span, ctx := tracer.StartSpanFromContext(req.Context(), "api.handle_request")
    defer span.Finish()

    // 2. Extract parameters
    params := mux.Vars(req)
    id := params["id"]
    span.SetTag("request_id", id)

    // 3. Parse body (if needed)
    var input RequestBody
    if err := json.NewDecoder(req.Body).Decode(&input); err != nil {
        span.Finish(tracer.WithError(err))
        http.Error(w, err.Error(), http.StatusBadRequest)
        return
    }
    defer req.Body.Close()

    // 4. Business logic
    result, err := a.service.Process(ctx, id, input)
    if err != nil {
        span.Finish(tracer.WithError(err))
        logger.Error(ctx, errx.Wrap(err, "processing request"))
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }

    // 5. Write response
    w.Header().Set("Content-Type", "application/json")
    if err := json.NewEncoder(w).Encode(result); err != nil {
        logger.Error(ctx, errx.Wrap(err, "encoding response"))
    }
}
```

### Middleware Template

```go
func (a *API) loggingMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        start := time.Now()
        reqID := uuid.NewString()

        logger.Info(r.Context(), "incoming request", logger.Fields{
            "method":   r.Method,
            "path":     r.URL.Path,
            "req_id":   reqID,
        })

        defer func() {
            logger.Debug(r.Context(), "request completed", logger.Fields{
                "duration_ms": time.Since(start).Milliseconds(),
                "req_id":      reqID,
            })
        }()

        next.ServeHTTP(w, r)
    })
}
```

## 5. Error Handling Patterns

### Sentinel Errors

```go
var (
    ErrNotFound     = errors.New("not found")
    ErrUnauthorized = errors.New("unauthorized")
    ErrConflict     = errors.New("conflict")
)

// Usage
if err == ErrNotFound {
    return nil, http.StatusNotFound
}
```

### Wrapped Errors with Context

```go
func (r *repo) GetUser(ctx context.Context, id string) (*User, error) {
    user, err := r.db.Find(ctx, id)
    if err != nil {
        return nil, errx.Wrap(err, "finding user", errx.Fields{
            "userId": id,
        })
    }
    return user, nil
}
```

### Named Returns for Complex Functions

```go
func (s *Service) ProcessBatch(ctx context.Context, items []Item) (results []Result, retErr error) {
    tx, err := s.db.Begin(ctx)
    if err != nil {
        return nil, errx.Wrap(err, "starting transaction")
    }

    defer func() {
        if retErr != nil {
            tx.Rollback()
        }
    }()

    // Process items...

    if err := tx.Commit(); err != nil {
        return nil, errx.Wrap(err, "committing transaction")
    }

    return results, nil
}
```

## 6. Concurrency Patterns

### Goroutine with Recovery

```go
go func() {
    defer logger.RecoverAndLog()

    for {
        select {
        case <-ctx.Done():
            return
        case msg := <-messages:
            if err := process(ctx, msg); err != nil {
                logger.Error(ctx, errx.Wrap(err, "processing message"))
            }
        }
    }
}()
```

### sync.Map for Concurrent Access

```go
type Controller struct {
    activeConferences  sync.Map  // confID -> *Conference
    pendingConferences sync.Map  // confID -> bool
}

func (c *Controller) GetConference(id string) *Conference {
    val, ok := c.activeConferences.Load(id)
    if !ok {
        return nil
    }
    return val.(*Conference)
}

func (c *Controller) StoreConference(id string, conf *Conference) {
    c.activeConferences.Store(id, conf)
}
```

### sync.Once for Cleanup

```go
type Service struct {
    closeOnce sync.Once
    quit      chan struct{}
}

func (s *Service) Close() error {
    s.closeOnce.Do(func() {
        close(s.quit)
    })
    return nil
}
```

## 7. Distributed Locking (etcd)

```go
func GetLease(ctx context.Context, cli *etcd.Client, resourceID string) (*Lock, error) {
    session, err := concurrency.NewSession(cli, concurrency.WithTTL(30))
    if err != nil {
        return nil, errx.Wrap(err, "creating etcd session")
    }

    mutex := concurrency.NewMutex(session, "/locks/"+resourceID)

    // Let context control timeout - don't add separate timeout
    if err := mutex.Lock(ctx); err != nil {
        session.Close()
        return nil, errx.Wrap(err, "acquiring lock")
    }

    return &Lock{mutex: mutex, session: session}, nil
}
```

## 8. Route Registration

```go
func NewAPIServer(config Config, clients Clients) (*API, error) {
    router := muxtrace.NewRouter(muxtrace.WithIgnoreRequest(func(r *http.Request) bool {
        return r.URL.Path == "/" || r.URL.Path == "/metrics"
    }))

    api := &API{config: config, clients: clients}

    // Apply middleware
    router.Use(api.authMiddleware)
    router.Use(api.loggingMiddleware)

    // Health check
    router.HandleFunc("/", api.healthcheck).Methods(http.MethodGet)

    // API routes
    router.HandleFunc("/api/v1/items", api.listItems).Methods(http.MethodGet)
    router.HandleFunc("/api/v1/items", api.createItem).Methods(http.MethodPost)
    router.HandleFunc("/api/v1/items/{id}", api.getItem).Methods(http.MethodGet)
    router.HandleFunc("/api/v1/items/{id}", api.deleteItem).Methods(http.MethodDelete)

    // Internal routes
    router.HandleFunc("/internal/api/v1/nodes", api.listNodes).Methods(http.MethodGet)

    api.Handler = router
    return api, nil
}
```
