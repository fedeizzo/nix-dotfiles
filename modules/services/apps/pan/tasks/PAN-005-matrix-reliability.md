---
id: PAN-005
title: Harden and test Matrix
status: done
priority: P1
owner: codex
created: 2026-09-12
updated: 2026-09-14
labels:
  - matrix
  - reliability
  - testing
depends_on: []
related:
  - PAN-001
  - PAN-002
---

# PAN-005: Harden and test Matrix

## Summary

Cover Matrix delivery, threading, retention, and recovery with integration tests and explicit retry behavior.

## Context

The current Matrix adapter persists encrypted SDK sessions, filters the configured sender and room, replies in threads, runs scheduled notifications, and redacts old messages. Tests currently cover only pure filtering and cron parsing; there is no mock-homeserver coverage.

## Scope

- Add a controllable mock Matrix homeserver or HTTP-level contract fixtures.
- Test login/session restoration, invite filtering, room filtering, thread-root selection, and encrypted-event handling.
- Add bounded send retry/backoff with Matrix rate-limit awareness.
- Test retention pagination, event-type filtering, and failure continuation.
- Specify recovery behavior when a pending workflow's Matrix root event was redacted or deleted.

## Out of Scope

- Supporting arbitrary Matrix users or public rooms.
- Replacing the Matrix SDK's encryption store.

## Acceptance Criteria

- [x] Integration tests verify that transaction and email confirmations bind to the correct root event.
- [x] HTTP 429 responses honor the server retry delay within a configured maximum.
- [x] A failure in one room or event does not abort retention for all rooms.
- [x] Historical messages are not passed to the LLM after startup.
- [x] Logs identify job and room without exposing message bodies.

## Implementation Notes

Design tests around the Matrix protocol boundary instead of coupling every test to SDK internals. Coordinate restart cases with [PAN-001](PAN-001-persist-workflow-state.md).

## Validation

```bash
CARGO_BUILD_JOBS=2 cargo test interface::matrix
CARGO_BUILD_JOBS=2 cargo clippy --all-targets --all-features -- -D warnings
```

## Progress

- 2026-09-12 — Card created from the Matrix integration-test and retry gaps.
- 2026-09-14 — Claimed by Codex. Adding protocol-boundary tests and explicit delivery/retention recovery behavior.
- 2026-09-14 — Verified retention isolates pagination and redaction failures per room/event, logs the failure with room/event identifiers, and continues the outer room loop.
- 2026-09-14 — Added Matrix protocol fixtures for thread and reply confirmation roots, plus a mock-homeserver 429 test. Delivery retries honor `retry_after_ms`, are limited to three attempts, and have a 30-second maximum retry window. Validation: `cargo test interface::matrix` and strict Clippy pass in the devshell.
