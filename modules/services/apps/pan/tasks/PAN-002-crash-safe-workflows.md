---
id: PAN-002
title: Make delivery and mutations crash-safe
status: in_progress
priority: P0
owner: codex
created: 2026-09-12
updated: 2026-09-12
labels:
  - reliability
  - idempotency
  - workflows
depends_on:
  - PAN-001
related:
  - PAN-005
---

# PAN-002: Make delivery and mutations crash-safe

## Summary

Add durable state transitions, idempotency, and reconciliation around Matrix delivery and confirmed Lunch Money/Fastmail mutations.

## Context

There are unavoidable process-crash windows between reserving a resource, sending a Matrix message, calling an external mutation, and recording completion. [PAN-001](PAN-001-persist-workflow-state.md) provides the durable state required to close those windows.

## Scope

- Define explicit workflow states and valid transitions.
- Record operation identifiers and attempts before external side effects.
- Reconcile ambiguous operations on startup without blindly repeating mutations.
- Retry transient failures with bounded exponential backoff and jitter.
- Preserve actionable failure information while excluding secrets and personal message bodies.
- Make multi-step Fastmail updates safe when one step succeeds and another fails.

## Out of Scope

- A general distributed workflow engine.
- Exactly-once guarantees that the external APIs cannot support.

## Acceptance Criteria

- [ ] Tests cover a simulated crash at every external-call boundary.
- [ ] A delivered Matrix review is not duplicated after restart when its delivery can be reconciled.
- [ ] Confirmed mutations are not blindly replayed after an ambiguous response.
- [ ] Transient failures retry; permanent validation failures remain visible and do not loop.
- [ ] Operators can identify and safely retry or abandon a failed workflow.

## Implementation Notes

Use at-least-once execution plus idempotent/reconciled operations where possible. Document API-specific limitations rather than promising exactly-once semantics.

## Validation

```bash
CARGO_BUILD_JOBS=2 cargo test
CARGO_BUILD_JOBS=2 cargo clippy --all-targets --all-features -- -D warnings
```

## Progress

- 2026-09-12 — Card created; blocked by the durable repository in PAN-001.
- 2026-09-12 — PAN-001 completed; claimed by Codex to add durable transitions, operation IDs, reconciliation, and bounded retries.
- 2026-09-12 — Implemented schema v2 and explicit `reserved → delivered → applying → completed/failed` transitions. Confirmed action payloads and operation IDs are persisted before external mutations; attempt counts, sanitized errors, and next-attempt timestamps are retained for failures.
- 2026-09-12 — Added startup reconciliation. Lunch Money operations are completed without replay when the exact transaction already reflects the confirmed update; ambiguous Fastmail operations are moved to a manual-retry failure instead of being replayed blindly.
- 2026-09-12 — Added bounded exponential retries for transient Lunch Money and Fastmail errors, plus a thread-level `cancel` path for safely abandoning pending work. Fastmail's mailbox-plus-seen sequence remains idempotent on retry but still needs explicit boundary-failure tests.
- 2026-09-12 — Added repository lifecycle coverage and a paused-time transient retry test. Strict Clippy passed immediately before the latest test additions. The subsequent test build was interrupted while recompiling dependencies after enabling Tokio `test-util`; rerun the full validation before checking any acceptance criterion or closing this card.
