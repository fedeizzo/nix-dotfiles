# Pan (Rust) Agent Instructions

## Architecture & Methodology

- **Hexagonal Architecture (DDD)**: This project strictly follows Hexagonal Architecture principles combined with Domain-Driven Design (DDD). Organize the code to separate the core domain logic from infrastructure, adapters, and external concerns.
- **Test-Driven Development (TDD)**: Adopt a TDD approach. Tests should be written or planned before implementing the core logic to ensure correctness and drive the design.

## Version Control

- Use Jujutsu (`jj`) for status, history, diffs, descriptions, and change management. Do not use Git commands for routine project work.
- Inspect the workspace with `jj status` and `jj diff` before editing or claiming a task.
- Preserve unrelated changes in the working-copy commit. Do not abandon, squash, rebase, or otherwise rewrite changes unless the user explicitly requests it.
- Use `jj describe -m "..."` only when asked to name the current change; do not create bookmarks or push changes without explicit authorization.

## Project Task Wiki

Project work is tracked as Markdown cards under [`tasks/`](../tasks/). Treat this as the shared source of truth for planned, active, blocked, and completed work; both humans and agents may edit it.

### Starting Work

1. Read [`tasks/README.md`](../tasks/README.md), then follow card links and dependencies.
2. Choose a `ready` card whose `depends_on` items are done. Do not silently start a `backlog`, `blocked`, or `done` card.
3. Before changing code, set `status: in_progress`, set `owner` to a useful human or agent identifier, update `updated`, and add a dated Progress entry.
4. Re-read the relevant code and verify that the card is still accurate. Update its Context or Scope when reality differs; record material scope changes in the Progress log.
5. Keep changes inside the card's Scope. Create or link a follow-up card instead of expanding the task substantially.

### Creating a Card

- Copy [`tasks/TEMPLATE.md`](../tasks/TEMPLATE.md) to `tasks/PAN-NNN-short-slug.md` using the next unused numeric ID. IDs and filenames never change after creation.
- Fill every metadata field. Use only these statuses: `backlog`, `ready`, `in_progress`, `blocked`, `done`, `cancelled`.
- Write observable acceptance criteria. Include commands or tests under Validation whenever possible.
- Link dependencies and related cards with relative Markdown links. A dependency means work should not start until that card is `done`; a related card is informational only.
- Add the card to the appropriate table in [`tasks/README.md`](../tasks/README.md) in the same change.

### Updating and Closing a Card

- Keep `updated` current and append concise, dated Progress entries; do not rewrite history merely because the plan changed.
- When blocked, set `status: blocked` and describe the exact unblock condition in Progress. When unblocked, return it to `ready` or `in_progress`.
- Before setting `status: done`, satisfy every acceptance criterion, run the listed validation, record results, and update documentation affected by the work.
- Move the card's row to the matching dashboard table whenever its status changes. Cards remain in the flat `tasks/` directory after completion so inbound links remain valid.
- If a card becomes obsolete, use `cancelled` and explain why; do not delete it.

### Coordination Rules

- One owner at a time. Check `jj status`, `jj diff`, and the card's Progress log before claiming work.
- Metadata is the current state; Progress is the audit trail; acceptance criteria define completion.
- Update cards as part of implementation, not as a separate cleanup pass.
- Never place credentials, personal transaction/email contents, access tokens, or other secrets in task cards.
