---
id: PAN-007
title: Decide and implement agent topology
status: done
priority: P2
owner: codex
created: 2026-09-12
updated: 2026-09-14
labels:
  - llm
  - architecture
  - safety
depends_on: []
related:
  - PAN-004
  - PAN-006
---

# PAN-007: Decide and implement agent topology

## Summary

Decide whether Pan should retain its single general Rust agent or restore the Go version's orchestrator and specialist-agent topology, then implement the smallest justified design.

## Context

[`Rig`](../src/infrastructure/ai.rs) currently exposes one agent with in-memory conversation history and read-only Lunch Money/Fastmail tools. The Go application has orchestrator, email, Lunch Money, and Fusion agents with dedicated prompts and streamed tool events. Deterministic Rust workflows deliberately own sensitive mutations.

## Scope

- Write a short decision record comparing one agent, routed specialists, and deterministic-only workflows.
- Define tool ownership, prompt boundaries, maximum turns, timeouts, and cancellation.
- Preserve the rule that external mutations require workflow-bound user confirmation.
- Add prompt/tool contract tests and representative evaluation cases for the chosen topology.
- Define how LLM conversation memory interacts with durable workflow state without conflating them.

## Out of Scope

- Fusion, Hindsight, telemetry, and prompt optimization unless separately carded.
- Giving the model direct access to credentials or unrestricted mutation tools.

## Acceptance Criteria

- [x] The topology decision and rejected alternatives are documented.
- [x] Every tool has an owning component and an explicit read/mutate classification.
- [x] Mutation authorization cannot be manufactured by a model tool call.
- [x] Timeouts and maximum tool turns are covered by tests.
- [x] Evaluation cases demonstrate correct routing and refusal behavior.

## Implementation Notes

Do not assume more agents are inherently better. Prefer deterministic application services for stable business workflows and use the LLM where interpretation materially improves the result.

## Validation

```bash
CARGO_BUILD_JOBS=2 cargo test
CARGO_BUILD_JOBS=2 cargo clippy --all-targets --all-features -- -D warnings
```

## Progress

- 2026-09-12 — Card created; decision intentionally deferred until core workflow durability is addressed.
- 2026-09-14 — Claimed by Codex after workflow durability work. Documenting and testing the single read-only assistant topology.
- 2026-09-14 — Added `docs/agent-topology.md` and a tool-access contract test. The assistant remains limited to five read-only tool turns; workflow boundaries own mutations.
- 2026-09-14 — Added a 30-second assistant timeout with paused-time coverage and documented ownership/access for every exposed tool group.
- 2026-09-14 — Added read-versus-mutation routing evaluation coverage; mutation access is refused by the assistant and directed to workflow confirmation.
