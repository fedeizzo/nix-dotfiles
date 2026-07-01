# Architecture

## Core Sections (Required)

### 1) Architectural Style

- Primary style: Multi-agent system / Event-driven
- Why this classification: Uses `google.golang.org/adk` for distinct agents (`orchestrator`, `lunchmoney`, `email`, etc.) communicating with users via bot interfaces. Relies on `samber/ro` for event streaming.
- Primary constraints: Agents need to safely execute tools (e.g. `dry_run` handling). Secrets/credentials must be loaded securely via file or command before tools can initialize.

### 2) System Flow

```text
[User Input (Matrix/CLI)] -> [Bot Interface] -> [Orchestrator Agent] -> [Target Agent] -> [Tool Execution] -> [Response]
```

1. Bot Interface (CLI or Matrix) captures user message.
2. Message is passed to the Orchestrator or designated runner.
3. The LLM processes the prompt/context and outputs tool execution requests.
4. If a tool is requested, the application executes it via ADK's `runner` and feeds the result back.
5. The final LLM response is returned to the user via the Bot interface.

### 3) Layer/Module Responsibilities

| Layer or module | Owns | Must not own | Evidence |
|-----------------|------|--------------|----------|
| `internal/bot` | Adapting CLI/Matrix to agent sessions | Business logic or tool execution | scan.txt |
| `internal/agent` | LLM interactions, prompts, role definitions | Direct API requests | scan.txt |
| `internal/tool` | Wrapping external APIs as ADK tools | LLM session management | scan.txt |
| `internal/optimizer` | Closed-loop test-driven prompt engineering | Production data mutation | `plan.md` |

### 4) Reused Patterns

| Pattern | Where found | Why it exists |
|---------|-------------|---------------|
| Agent definition | `internal/agent/*` | Each sub-folder contains `[name].go` and `prompt.md` to cleanly separate LLM instructions from Go logic. |
| Tool integration | `internal/tool/*` | Wrapping external SDKs (like `lunchmoney-go`) into a standardized format for the LLM to call. |

### 5) Known Architectural Risks

- State Mutation in Optimization Loop: `pan optimize` executes tools in a `dry_run` mode; failure to strictly enforce this could cause catastrophic data mutation on real accounts.
- Missing Agent Guardrails: Unbounded tool execution loops or recursive sub-agent delegation.

### 6) Evidence

- `plan.md`
- `internal/app/app.go`
- `internal/bot/cli/cli_test.go`
