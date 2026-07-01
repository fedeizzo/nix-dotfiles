# AGENTS.md

## Project Overview

`pan` is an Agentic AI CLI and Bot (with Matrix integration) built in Go 1.26.3. It leverages the Agent Development Kit (`google.golang.org/adk`) to coordinate multiple specialized agents (e.g., `lunchmoney`, `email`, `orchestrator`, `fusion`). A core feature of this project is the test-driven prompt engineering loop (`pan optimize`), which iteratively refines an agent's `prompt.md` using an LLM meta-agent by executing behavioral test scenarios.

## Setup Commands

- **Install dependencies:** `go mod tidy`
- **Build the CLI binary:** `go build -o pan ./cmd/pan`
- **Mock Generation:** Uses `mockery` for test mocks (configured via `.mockery.yml`).

## Development Workflow

- The project contains a `Justfile` for simplified command running.
- **Configuration:** Managed via `viper` (`internal/config/config.go`). You can provide secrets using either a file path (e.g., `FASTMAIL_API_FILE`) or a shell command (e.g., `FASTMAIL_API_CMD`).
- **Dependencies:** The application uses `samber/ro` for event streaming and `samber/oops` for structured error handling and stack traces.

## Testing Instructions

- **Run all tests:** `go test ./...`
- **Generate coverage report:** `go test -coverprofile=coverage.out ./...` (No hard coverage threshold is enforced).
- **Mocking:** Interfaces are mocked via `mockery`. The generated files are stored in `mocks/` subdirectories alongside the target package.
- **Testing specifics:** 
  - There are no E2E tests for the Matrix integration; rely on unit tests and interface boundaries.
  - The optimizer tool (`cmd/pan/optimize.go`) executes the agents against behavioral tests. 

## Code Style and Conventions

- **Formatting:** Standard Go conventions (`go fmt ./...`).
- **Error Handling:** Wrap errors with domains using `oops`: `oops.In("domain").Wrapf(err, "message")`.
- **File Naming:** `snake_case` for all Go source and test files.
- **Structure:** `cmd/pan/` contains entrypoints, `internal/app/` holds application wiring/DI, `internal/agent/` contains specific ADK agent definitions and their `prompt.md` files, and `internal/tool/` wraps executable API tools.

## Security & Mutation Safety (CRITICAL)

- **Agent Mutation Safety:** Because `pan optimize` executes agents automatically in a loop, **mutating tools must be safe**. Every tool that mutates state (e.g., updating a financial record, sending an email) MUST check the Go context for a `dry_run` flag.
  - If `context.Value("dry_run") == true`, the tool must simulate success and skip the actual API mutation. 
  - Read-only tools are allowed to hit real APIs to provide grounded context to the optimizer.
- **Credentials:** Never hardcode secrets. Always use the configuration abstraction which reads from files or executes shell commands.

## Additional Notes

- **Database:** Local memory and agent states are stored using SQLite (`github.com/mattn/go-sqlite3`).
- **Telemetry:** Built-in tracing using OpenTelemetry (`otlptracehttp`).
- **Homelab:** The project is integrated closely with NixOS and Home Manager (evidenced by `pan.n.nix` and `module.nix`); configuration tweaks often propagate to a larger homelab deployment.
