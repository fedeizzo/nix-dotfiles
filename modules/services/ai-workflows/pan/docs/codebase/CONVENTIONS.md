# Coding Conventions

## Core Sections (Required)

### 1) Naming Rules

| Item | Rule | Example | Evidence |
|------|------|---------|----------|
| Files | snake_case | `config.go`, `cli_test.go` | scan.txt |
| Functions/methods | camelCase (private), PascalCase (public) | `buildAgents`, `Execute` | `internal/app/app.go`, `cmd/pan/main.go` |
| Types/interfaces | PascalCase | `Config`, `Bot` | `internal/config/config.go` |
| Constants/env vars | UPPER_SNAKE_CASE | `FASTMAIL_API_FILE` | `internal/config/config.go` |

### 2) Formatting and Linting

- Formatter: `gofmt` (default Go toolchain, no explicit config found)
- Linter: `golangci-lint` (assumed standard, no config found)
- Most relevant enforced rules: Standard Go formatting
- Run commands: `go fmt ./...`

### 3) Import and Module Conventions

- Import grouping/order: Group 1: stdlib, Group 2: third-party dependencies, Group 3: local internal packages.
- Alias vs relative import policy: Absolute module paths (`pan/internal/config`).
- Public exports/barrel policy: Interfaces/types exposed directly from package root (`internal/config`).

### 4) Error and Logging Conventions

- Error strategy by layer: Uses `github.com/samber/oops` for structured error wrapping with stack traces, setting the domain (`oops.In("domain").Wrapf(err, "msg")`).
- Logging style and required context fields: [TODO] Review `internal/telemetry` for exact log schema.
- Sensitive-data redaction rules: [TODO]

### 5) Testing Conventions

- Test file naming/location rule: Co-located with source (`*_test.go`).
- Mocking strategy norm: Generated via `mockery`, stored in `mocks/` subdirectories (`internal/lunchmoney/mocks`).
- Coverage expectation: No target threshold.

### 6) Evidence

- `internal/config/config.go`
- `internal/app/app.go`
- `internal/bot/cli/cli_test.go`
- `.mockery.yml`
