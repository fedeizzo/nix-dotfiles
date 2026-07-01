# External Integrations

## Core Sections (Required)

### 1) Integration Inventory

| System | Type (API/DB/Queue/etc) | Purpose | Auth model | Criticality | Evidence |
|--------|---------------------------|---------|------------|-------------|----------|
| Fastmail | API (JMAP) | Email agent interactions | API Key | High | `internal/config/config.go`, `go.mod` |
| LunchMoney | API | Financial tracking agent | API Key | High | `internal/config/config.go`, `go.mod` |
| Matrix | Chat Protocol | Bot user interface | Username/Password | High | `internal/config/config.go`, `go.mod` |
| OpenAI API | LLM API | AI Agent brain | API Key | High | `internal/config/config.go`, `go.mod` |
| Fusion | API | RSS Reader | Password | Low | `internal/config/config.go` |
| Hindsight | API | [TODO] Telemetry or sync | API Key | Low | `internal/config/config.go`, `go.mod` |
| OpenTelemetry | Telemetry (OTLP) | Observability/Tracing | None (internal) | Low | `go.mod` |

### 2) Data Stores

| Store | Role | Access layer | Key risk | Evidence |
|-------|------|--------------|----------|----------|
| SQLite3 | Local storage / agent memory | `database/sql` / ORM? [TODO] | Concurrency locking limits | `go.mod` |
| File System | `conversation.json` / `pan.log` | Raw logs and state | Disk corruption on crash | `docs/codebase/.codebase-scan.txt` |

### 3) Secrets and Credentials Handling

- Credential sources: `config.yaml` combined with ENV overrides. Secrets are specifically sourced via file (`*_FILE`) or shell command execution (`*_CMD`) using `resolveSecret` to avoid hardcoding.
- Hardcoding checks: None found.
- Rotation or lifecycle notes: Unknown, managed via Homelab NixOS secrets (`pan-homelab-secrets.yaml`).

### 4) Reliability and Failure Behavior

- Retry/backoff behavior: `github.com/cenkalti/backoff/v5` is present in `go.sum`, indicating retries are implemented.
- Timeout policy: [TODO]
- Circuit-breaker or fallback behavior: [TODO]

### 5) Observability for Integrations

- Logging around external calls: OpenTelemetry is used heavily (`internal/telemetry/http.go`).
- Metrics/tracing coverage: Tracing enabled via `otlptracehttp`.
- Missing visibility gaps: [TODO]

### 6) Evidence

- `internal/config/config.go`
- `go.mod`
- `go.sum`
