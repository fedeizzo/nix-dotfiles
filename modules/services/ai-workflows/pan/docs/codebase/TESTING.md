# Testing Patterns

## Core Sections (Required)

### 1) Test Stack and Commands

- Primary test framework: Go built-in `testing` library.
- Assertion/mocking tools: `github.com/stretchr/testify`, `mockery`.
- Commands:

```bash
go test ./...
go test -v ./...
go tool cover -html=coverage.out
```

### 2) Test Layout

- Test file placement pattern: Co-located with source code (`*_test.go`).
- Naming convention: `Test[FunctionName]` (e.g., `TestBuildMessage`).
- Setup files and where they run: Within individual test suites.

### 3) Test Scope Matrix

| Scope | Covered? | Typical target | Notes |
|-------|----------|----------------|-------|
| Unit | yes | CLI parsers, config loaders, logic | E.g. `cli_test.go` |
| Integration | yes | `optimizer` | Uses dry run context (`plan.md`) |
| E2E | no | Full bot flows | No E2E tests for Matrix integration |

### 4) Mocking and Isolation Strategy

- Main mocking approach: Interfaces are mocked using generated code from `mockery`. Mocks are placed in `mocks/` directories (e.g., `internal/lunchmoney/mocks/mock_Service.go`).
- Isolation guarantees: Unit tests use isolated instances.
- Common failure mode in tests: [TODO]

### 5) Coverage and Quality Signals

- Coverage tool + threshold: Go standard coverage (`go test -coverprofile=coverage.out`). No target coverage threshold enforced.
- Current reported coverage: `coverage.out` exists, implying active tracking.
- Known gaps/flaky areas: [TODO]

### 6) Evidence

- `internal/bot/cli/cli_test.go`
- `.mockery.yml`
- `docs/codebase/.codebase-scan.txt`
