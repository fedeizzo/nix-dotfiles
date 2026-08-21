---
id: PAN-003
title: Complete the scheduler registry
status: ready
priority: P1
owner: unassigned
created: 2026-09-12
updated: 2026-09-12
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

- [ ] Invalid runner, condition, and cron values fail startup with the job name in the error.
- [ ] Conditions determine whether a job runs.
- [ ] Multiple jobs execute independently while sharing resource reservations.
- [ ] Scheduler tests do not depend on wall-clock waiting.
- [ ] Configuration documentation explains every job field.

## Implementation Notes

Keep cron parsing and dispatch independent of Matrix; delivery should be an injected port. Avoid turning deterministic financial review into an LLM prompt merely to consume the existing `prompt` field.

## Validation

```bash
CARGO_BUILD_JOBS=2 cargo test interface::matrix
CARGO_BUILD_JOBS=2 cargo clippy --all-targets --all-features -- -D warnings
```

## Progress

- 2026-09-12 — Card created after identifying ignored job fields.
