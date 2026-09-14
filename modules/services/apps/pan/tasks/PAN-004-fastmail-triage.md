---
id: PAN-004
title: Complete Fastmail triage
status: done
priority: P1
owner: codex
created: 2026-09-12
updated: 2026-09-14
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

- [x] A schedule can deliver an unread email triage card to Matrix.
- [x] Concurrent threads never claim the same email and can advance to later unread messages.
- [x] Triage output follows a validated schema and clearly distinguishes suggestions from applied actions.
- [x] No Fastmail mutation occurs without explicit confirmation.
- [x] Message-size limits and redaction/logging rules are tested.

## Implementation Notes

Do not give a general LLM tool an unrestricted confirmation boolean. Confirmation must originate from the Matrix/CLI workflow boundary and bind to an exact pending action.

## Validation

```bash
CARGO_BUILD_JOBS=2 cargo test application::email_triage infrastructure::fastmail
CARGO_BUILD_JOBS=2 cargo clippy --all-targets --all-features -- -D warnings
```

## Progress

- 2026-09-12 — Card created from Fastmail parity gaps.
- 2026-09-14 — Claimed by Codex. Added bounded unread-email selection that skips existing durable claims, and wired the `fastmail:has_unread` scheduler path. Continuing with triage schema, privacy limits, and scheduler boundary tests.
- 2026-09-14 — Added serialized confirmation-required triage suggestions, rendered explicitly as not applied, with schema coverage.
- 2026-09-14 — Verified the Fastmail scheduler claims, delivers, binds, and releases triage workflows through the Matrix delivery path. Validation: focused triage tests and strict Clippy passed in the devshell.
