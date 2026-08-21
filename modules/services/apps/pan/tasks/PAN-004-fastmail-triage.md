---
id: PAN-004
title: Complete Fastmail triage
status: ready
priority: P1
owner: unassigned
created: 2026-09-12
updated: 2026-09-12
labels:
  - fastmail
  - workflow
depends_on: []
related:
  - PAN-001
  - PAN-003
---

# PAN-004: Complete Fastmail triage

## Summary

Turn the existing manual Fastmail confirmation path into a complete, schedulable triage workflow with richer message data and safe selection.

## Context

The Rust backend can list mailboxes, fetch one unread message, add a mailbox, and mark a message seen. Matrix users can start the deterministic flow with `email: Inbox`. Unlike the Go implementation, structured triage suggestions and scheduled inbox processing are absent.

## Scope

- Fetch enough message content for useful triage while applying explicit size and privacy limits.
- Select the next unreserved unread email rather than stopping when the first result is active elsewhere.
- Add a scheduled Fastmail runner and `fastmail:has_unread` condition.
- Define a structured triage suggestion domain type and rendering.
- Keep mailbox/tag and seen mutations behind explicit user confirmation.
- Define behavior for partial mailbox-plus-seen failures.

## Out of Scope

- Automatically creating calendar events or todo items.
- Attachment downloading unless separately approved and carded.

## Acceptance Criteria

- [ ] A schedule can deliver an unread email triage card to Matrix.
- [ ] Concurrent threads never claim the same email and can advance to later unread messages.
- [ ] Triage output follows a validated schema and clearly distinguishes suggestions from applied actions.
- [ ] No Fastmail mutation occurs without explicit confirmation.
- [ ] Message-size limits and redaction/logging rules are tested.

## Implementation Notes

Do not give a general LLM tool an unrestricted confirmation boolean. Confirmation must originate from the Matrix/CLI workflow boundary and bind to an exact pending action.

## Validation

```bash
CARGO_BUILD_JOBS=2 cargo test application::email_triage infrastructure::fastmail
CARGO_BUILD_JOBS=2 cargo clippy --all-targets --all-features -- -D warnings
```

## Progress

- 2026-09-12 — Card created from Fastmail parity gaps.
