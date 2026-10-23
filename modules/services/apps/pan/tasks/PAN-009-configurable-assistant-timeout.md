---
id: PAN-009
title: Make the assistant timeout configurable
status: done
priority: P1
owner: codex
created: 2026-10-23
updated: 2026-10-23
labels: [configuration, llm]
depends_on: []
related: []
---

# PAN-009: Make the assistant timeout configurable

## Summary

Configure the whole assistant request timeout through `models.timeout_seconds`.

## Scope

Preserve the 30-second default, reject zero, wire the setting into Rig, document it, and set the local configuration to 180 seconds.

## Acceptance Criteria

- [x] Missing values default to 30 seconds and zero is rejected.
- [x] The configured deadline controls the entire assistant operation.
- [x] Local configuration and README document 180 seconds.
- [x] Tests, formatting, and strict Clippy pass.

## Validation

```sh
cargo fmt --check
cargo test --locked
cargo clippy --locked --all-targets --all-features -- -D warnings
```

## Progress

- 2026-10-23 — Created and started at the user's request. Test plan: deserialize default/explicit/zero values, and use paused Tokio time to check completion and expiration at a custom deadline.

- 2026-10-23 — Completed: positive timeout configuration with a 30-second default, Rig wiring, local 180-second setting, and documentation. Validation: 79 tests passed (mock HTTP tests run outside the sandbox), formatting and strict Clippy passed.
