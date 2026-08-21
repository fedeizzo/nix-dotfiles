---
id: PAN-001
title: Persist workflow state
status: done
priority: P0
owner: codex
created: 2026-09-12
updated: 2026-09-12
labels:
  - architecture
  - persistence
  - workflows
depends_on: []
related:
  - PAN-002
  - PAN-005
---

# PAN-001: Persist workflow state

## Summary

Store active Lunch Money and Fastmail workflows durably so reservations and Matrix-thread associations survive process restarts and can be shared safely by concurrent workers.

## Context

[`TransactionReviewService`](../src/application/transaction_review.rs) and [`EmailTriageService`](../src/application/email_triage.rs) currently coordinate threads with process-local mutex-protected maps. This prevents duplicate selection inside one running process, but a restart can resend an unanswered item and multiple Pan instances cannot coordinate.

## Scope

- Define a domain-facing workflow repository port.
- Add a SQLite implementation stored below Pan's configured data directory.
- Persist resource ID, workflow kind/state, Matrix room/thread IDs, expiry, and timestamps.
- Claim the next Lunch Money transaction atomically with a uniqueness constraint.
- Restore pending transaction and email confirmations after restart.
- Expire abandoned claims deterministically.

## Out of Scope

- Crash-safe external delivery and mutation orchestration; see [PAN-002](PAN-002-crash-safe-workflows.md).
- PostgreSQL or multi-host deployment.

## Acceptance Criteria

- [x] Restarting Pan does not cause a non-expired transaction already sent to Matrix to be selected again.
- [x] Two concurrent claim attempts cannot reserve the same external resource.
- [x] A reply received after restart resolves to the correct transaction or email.
- [x] Expired claims become eligible again and are removed or archived predictably.
- [x] Database schema creation and migration are automatic and tested.
- [x] No transaction or email content beyond what is operationally necessary is persisted.

## Implementation Notes

Keep SQLite behind an application/domain port; Matrix and API clients should not execute SQL. Prefer database-enforced uniqueness over a read-then-write check. Decide whether Matrix room ID must be part of the conversation key, rather than assuming event IDs alone are sufficient.

## Validation

```bash
CARGO_BUILD_JOBS=2 cargo test
CARGO_BUILD_JOBS=2 cargo clippy --all-targets --all-features -- -D warnings
```

Add restart and concurrent-claim integration tests using a temporary database.

## Progress

- 2026-09-12 — Card created from the production-readiness review.
- 2026-09-12 — Claimed by Codex; implementation started with the persistence port and restart/concurrency tests.
- 2026-09-12 — Completed with a SQLite repository, composite Matrix conversation keys, exact-record rehydration, and restart tests for Lunch Money and Fastmail. Validation: 55 tests passed; strict Clippy and formatting passed.
