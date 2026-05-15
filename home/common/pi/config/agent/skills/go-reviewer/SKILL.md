---
name: go-reviewer
description: "Review Go code against Chatlayer team conventions and Go best practices. Use when: (1) reviewing Go code changes, (2) checking Go PRs/MRs, (3) validating Go patterns, (4) ensuring Go idioms. Triggers: 'review go', 'go code review', 'check go code', 'go patterns', 'review *.go'."
compatibility: opencode
---

# Go Code Reviewer

Review Go code against Chatlayer team conventions derived from actual MR reviews and codebase patterns.

## Quick Review Checklist

### Naming (High Priority)

| Pattern            | Correct                    | Incorrect                   |
| ------------------ | -------------------------- | --------------------------- |
| Receiver names     | `func (a *API) Get()`      | `func (api *API) Get()`     |
| Acronyms           | `API`, `ID`, `URL`, `HTTP` | `Api`, `Id`, `Url`, `Http`  |
| Mutex suffix       | `streamUpdateMu`           | `streamLock`, `streamMutex` |
| Interface location | In consuming package       | In implementing package     |

### Error Handling

```go
// CORRECT: Wrap with context
return errx.Wrap(err, "fetching user", errx.Fields{"userId": id})

// INCORRECT: Bare return
return err

// CORRECT: Named return for complex functions
func process() (result Result, retErr error) {
    defer func() { if retErr != nil { cleanup() } }()
}
```

### Context & Tracing

```go
// CORRECT: Full pattern
func (a *API) Handle(w http.ResponseWriter, req *http.Request) {
    span, ctx := tracer.StartSpanFromContext(req.Context(), "api.handle")
    defer span.Finish()

    // Pass ctx to ALL downstream calls
    result, err := a.service.Process(ctx, input)
    if err != nil {
        span.Finish(tracer.WithError(err))
        logger.Error(ctx, errx.Wrap(err, "processing failed"))
        return
    }
}
```

### Imports (Order Matters)

```go
import (
    // 1. Standard library
    "context"
    "fmt"

    // 2. External packages
    "github.com/gorilla/mux"

    // 3. Internal packages
    "chatlayer.ai/common/errx"
    "chatlayer.ai/common/logger"
)
```

## Review Categories

### 1. Idiomatic Go

- [ ] Short receiver names (1-2 chars): `a`, `s`, `r`, not `api`, `service`, `repo`
- [ ] Proper acronym caps: `API`, `HTTP`, `ID`, `URL`
- [ ] Interface compliance check: `var _ Interface = (*impl)(nil)`
- [ ] Constructors return interface: `func New() (Interface, error)`

### 2. Error Handling

- [ ] All errors wrapped with `errx.Wrap(err, "context")`
- [ ] Sentinel errors defined: `var ErrNotFound = errors.New("not found")`
- [ ] No bare `return err` without context
- [ ] Named return values for complex error flows

### 3. Logging

- [ ] Using `chatlayer.ai/common/logger` (not `log` or `fmt.Printf`)
- [ ] Context passed: `logger.Info(ctx, "msg", logger.Fields{...})`
- [ ] Errors wrapped before logging: `logger.Error(ctx, errx.Wrap(err, "op"))`
- [ ] Service prefix in messages: `"voice_gateway: operation"`

### 4. Concurrency

- [ ] Context passed to all goroutines
- [ ] `defer logger.RecoverAndLog()` in goroutines
- [ ] `sync.Once` for one-time operations
- [ ] `sync.Map` for concurrent map access
- [ ] Channels for signaling: `quit := make(chan struct{})`

### 5. HTTP Handlers

- [ ] Tracing span created and deferred
- [ ] `defer req.Body.Close()` after reading
- [ ] `Content-Type` header set before writing
- [ ] `mux.Vars(req)` for path parameters
- [ ] Error responses use standard format

### 6. Testing

- [ ] Tests colocated in `*_test.go`
- [ ] Table-driven with `t.Run()`
- [ ] Using `testify/assert` or `gotest.tools/assert`
- [ ] Mock compliance: `var _ Interface = (*mock)(nil)`
- [ ] Integration tests tagged: `//go:build skip`

## Anti-Patterns to Flag

### Critical (Block MR)

```go
// FORBIDDEN: context.WithoutCancel
ctx = context.WithoutCancel(ctx)  // Never use

// FORBIDDEN: log.Fatal outside main.go
log.Fatal(err)  // Only in cmd/*/main.go

// FORBIDDEN: Suppressing errors
_ = dangerousOperation()  // Must handle error
```

### High Priority

```go
// BAD: Empty catch
if err != nil {
    // Silent failure
}

// BAD: Magic numbers
if len(items) > 100 {  // What is 100?

// BAD: Verbose receiver
func (controller *ConferenceController) Handle()  // Use (c *ConferenceController)
```

### Medium Priority

```go
// PREFER: Let context control timeout (not separate timeout param)
func GetLease(ctx context.Context, id string) (*Lock, error)
// OVER
func GetLease(ctx context.Context, id string, timeout time.Duration) (*Lock, error)

// PREFER: Descriptive variable names
active_conferences sync.Map
pending_conferences sync.Map
// OVER
conferences sync.Map
replicas sync.Map
```

## Comment Format

```
(AI assisted) **[Category]**: Issue description.

**Current**:
`problematic_code()`

**Suggested**:
`improved_code()`

**Why**: Brief explanation of the improvement.
```

Categories: `Go Idiom`, `Error Handling`, `Concurrency`, `Performance`, `Testing`, `Style`

## Reference Files

For detailed patterns, see:

- [PATTERNS.md](references/PATTERNS.md) - Complete code patterns with examples
- [TESTING.md](references/TESTING.md) - Testing conventions and examples
