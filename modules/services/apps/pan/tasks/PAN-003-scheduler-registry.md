---
id: PAN-003
title: Complete the scheduler registry
status: done
priority: P1
owner: codex
created: 2026-09-12
updated: 2026-09-14
labels:
  - scheduler
  - configuration
depends_on: []
related:
  - PAN-004
---

# PAN-003: Complete the scheduler registry

## Summary

Replace Lunch Money-specific job filtering with validated runner and condition registries that make every configured job field meaningful.

## Context

[`matrix.rs`](../src/interface/matrix.rs) runs concurrent jobs whose `runner` is `lunchmoney`. The configured `condition` and `prompt` fields are not evaluated, and unknown runners are skipped. The Go implementation has separate runner and evaluator registries.

## Scope

- Introduce typed runner and condition interfaces in the application layer.
- Validate runner names, condition names, and cron expressions during startup.
- Implement `lunchmoney:has_unreviewed` without duplicating a costly fetch unnecessarily.
- Decide whether deterministic runners should reject or ignore `prompt`; encode that choice in configuration types.
- Make scheduler timing testable with paused Tokio time.
- Report job start, skip, completion, and failure consistently.

## Out of Scope

- Fusion/RSS support.
- Persistent retry orchestration covered by [PAN-002](PAN-002-crash-safe-workflows.md).

## Acceptance Criteria

- [x] Invalid runner, condition, and cron values fail startup with the job name in the error.
- [x] Conditions determine whether a job runs.
- [x] Multiple jobs execute independently while sharing resource reservations.
- [x] Scheduler tests do not depend on wall-clock waiting.
- [x] Configuration documentation explains every job field.

## Implementation Notes

Keep cron parsing and dispatch independent of Matrix; delivery should be an injected port. Avoid turning deterministic financial review into an LLM prompt merely to consume the existing `prompt` field.

## Validation

```bash
CARGO_BUILD_JOBS=2 cargo test interface::matrix
CARGO_BUILD_JOBS=2 cargo clippy --all-targets --all-features -- -D warnings
```

## Progress

- 2026-09-12 — Card created after identifying ignored job fields.
- 2026-09-14 — Claimed by Codex. Implementing typed startup validation and deterministic Lunch Money scheduling first; Fastmail scheduling will be completed with PAN-004's triage selection work.
- 2026-09-14 — Added startup validation for the supported runner/condition pairs and five-, six-, or seven-field cron expressions. Invalid configuration reports the job name; deterministic runners document that `prompt` is ignored. Validation: 65 tests and strict Clippy passed. Fastmail dispatch remains pending PAN-004.
- 2026-09-14 — Added `docs/scheduler.md`, including every job field and the atomic prepare-based condition semantics.
- 2026-09-14 — Extracted scheduler delay calculation and added paused-Tokio-time coverage.
- 2026-09-14 — Verified scheduler dispatch uses one independent JoinSet task per validated runner and only delivers a card when the runner's atomic prepare operation reserves eligible work.
