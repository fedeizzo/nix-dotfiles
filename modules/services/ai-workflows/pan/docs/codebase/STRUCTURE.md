# Codebase Structure

## Core Sections (Required)

### 1) Top-Level Map

| Path | Purpose | Evidence |
|------|---------|----------|
| cmd/pan/ | CLI entry points and commands | scan.txt |
| internal/ | Non-exported application code and packages | scan.txt |
| docs/ | Documentation | scan.txt |
| module.nix / pan.n.nix | Nix integration and deployment configuration | scan.txt |

### 2) Entry Points

- Main runtime entry: `cmd/pan/main.go`
- Secondary entry points (worker/cli/jobs): `cmd/pan/optimize.go` (CLI optimization command)
- How entry is selected (script/config): By executing the compiled binary with `cobra` subcommands.

### 3) Module Boundaries

| Boundary | What belongs here | What must not be here |
|----------|-------------------|------------------------|
| internal/app | Application lifecycle, dependency injection (`Build` func) | Domain logic |
| internal/bot | Interface implementations (Matrix, CLI) | Core agent workflows |
| internal/agent | Defined ADK agents and their specific `prompt.md` files | HTTP/Network logic |
| internal/tool | Executable tools attached to agents | Agent decision logic |
| internal/config | Configuration parsing (`viper`) | Business logic |
| internal/telemetry | Logging, tracing, and metrics | Core business logic |

### 4) Naming and Organization Rules

- File naming pattern: snake_case for Go files (e.g., `main.go`, `cli_test.go`).
- Directory organization pattern: By technical layer (e.g., `agent/`, `tool/`, `bot/`, `app/`).
- Import aliasing or path conventions: Standard Go module paths (e.g., `pan/internal/config`).

### 5) Evidence

- `docs/codebase/.codebase-scan.txt`
- `internal/app/app.go`
- `cmd/pan/main.go`
