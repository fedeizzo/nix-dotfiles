---
id: PAN-006
title: Complete CLI and HTTP interfaces
status: ready
priority: P2
owner: unassigned
created: 2026-09-12
updated: 2026-09-12
labels:
  - cli
  - http
  - interface
depends_on: []
related:
  - PAN-001
---

# PAN-006: Complete CLI and HTTP interfaces

## Summary

Make deterministic workflows testable and operable outside Matrix, and either expose or remove the currently unwired HTTP interface.

## Context

`review-transaction` provides a read-only CLI preview, while the interactive CLI only sends messages to the general agent and keeps memory in process. `cli.conversation_path` is unused. HTTP handler code exists in [`api.rs`](../src/interface/api.rs), but `main` cannot start it and it has no authentication design.

## Scope

- Add interactive CLI confirmation for a precisely identified pending transaction.
- Decide and implement the semantics of `cli.conversation_path`, or remove the option.
- Add CLI commands for read-only connectivity/status checks without requiring unrelated services.
- Decide whether the HTTP interface is supported; if yes, add configuration, authentication, startup, graceful shutdown, and workflow endpoints; if no, remove dead code and config assumptions.
- Keep machine-readable output available for scripting.

## Out of Scope

- A browser frontend.
- Public Internet exposure without a separate security review.

## Acceptance Criteria

- [ ] Lunch Money preview can run without connecting to Fastmail, Matrix, or the LLM.
- [ ] A CLI mutation requires an explicit, transaction-bound confirmation.
- [ ] Exit codes distinguish success, no work, configuration errors, and provider failures.
- [ ] The HTTP interface is either fully wired and authenticated or cleanly removed.
- [ ] Help output and example commands are documented and tested.

## Implementation Notes

Configuration parsing currently requires sections that a narrow subcommand may not need. Consider command-specific validation and lazy adapter construction.

## Validation

```bash
CARGO_BUILD_JOBS=2 cargo test interface::cli interface::api
CARGO_BUILD_JOBS=2 cargo run -- --help
CARGO_BUILD_JOBS=2 cargo clippy --all-targets --all-features -- -D warnings
```

## Progress

- 2026-09-12 — Card created from CLI and unwired HTTP gaps.
