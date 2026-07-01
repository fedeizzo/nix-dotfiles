# Technology Stack

## Core Sections (Required)

### 1) Runtime Summary

| Area | Value | Evidence |
|------|-------|----------|
| Primary language | Go 1.26.3 | go.mod |
| Runtime + version | Go 1.26.3 | go.mod |
| Package manager | go mod | go.mod |
| Module/build system | go build / Justfile | go.mod, Justfile |

### 2) Production Frameworks and Dependencies

| Dependency | Version | Role in system | Evidence |
|------------|---------|----------------|----------|
| google.golang.org/adk | v1.4.0 | Core agent framework | go.mod |
| github.com/spf13/viper | v1.21.0 | Configuration management | go.mod, internal/config/config.go |
| github.com/spf13/cobra | v1.10.2 | CLI framework | go.sum, cmd/pan/optimize.go |
| github.com/samber/ro | v0.3.0 | Event/streaming system | go.mod, internal/app/app.go |
| maunium.net/go/mautrix | v0.28.0 | Matrix bot integration | go.mod |
| github.com/Cidan/lunchmoney-go | v0.0.0 | LunchMoney API integration | go.mod |
| github.com/mattn/go-sqlite3 | v1.14.44 | Local database | go.mod |
| github.com/robfig/cron/v3 | v3.0.1 | Job scheduling | go.mod |
| github.com/samber/oops | v1.22.0 | Structured error handling | go.mod, internal/config/config.go |

### 3) Development Toolchain

| Tool | Purpose | Evidence |
|------|---------|----------|
| github.com/stretchr/testify | TEST | go.mod, internal/bot/cli/cli_test.go |
| mockery (implied by .mockery.yml) | MOCK GENERATION | .mockery.yml |

### 4) Key Commands

```bash
go mod tidy
go build -o pan ./cmd/pan
go test ./...
```

### 5) Environment and Config

- Config sources: `config.yaml`, `config.example.yaml`, Environment variables.
- Required env vars: `FASTMAIL_API_FILE`, `MODEL_NAME`, `OPENAI_API_KEY`, `OPENAI_BASE_URL`, `INTERFACE`, `MATRIX_HOMESERVER`, `MATRIX_USER`, `MATRIX_PASSWORD_FILE`, `LUNCHMONEY_API_FILE`, `FUSION_ENDPOINT`, etc.
- Deployment/runtime constraints: Configuration bindings are defined in `internal/config/config.go`. Secrets can be loaded via file or executed command.

### 6) Evidence

- `go.mod`
- `internal/config/config.go`
- `cmd/pan/optimize.go`
