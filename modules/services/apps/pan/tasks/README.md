# Pan Task Wiki

This directory is Pan's lightweight project-management system. Each task is a stable Markdown card with machine-readable metadata, explicit acceptance criteria, and an append-only progress log. The cards are intentionally usable with ordinary editors, Jujutsu (`jj`), and coding agents—no external tracker is required.

## Workflow

```text
backlog -> ready -> in_progress -> done
                 -> blocked -> ready/in_progress
any non-final state -> cancelled
```

- `backlog`: worthwhile but not sufficiently prioritized or specified.
- `ready`: specified, prioritized, and free of unfinished dependencies.
- `in_progress`: actively owned; code or investigation is underway.
- `blocked`: cannot proceed until the Progress log's unblock condition is met.
- `done`: every acceptance criterion and validation item is satisfied.
- `cancelled`: intentionally abandoned, with the reason preserved.

See [TEMPLATE.md](TEMPLATE.md) for the card schema. Detailed create/update rules live in [AGENTS.md](../.agents/AGENTS.md#project-task-wiki).

## Card Metadata

- `id`: permanent `PAN-NNN` identifier; never reuse an ID.
- `status`: current workflow state using only the vocabulary above.
- `priority`: `P0` blocks safe deployment or risks data correctness; `P1` is important near-term work; `P2` is planned improvement; `P3` is an idea with no commitment.
- `owner`: the one active contributor, or `unassigned`. Ownership is coordination, not authorship.
- `created` and `updated`: ISO `YYYY-MM-DD` dates.
- `labels`: lowercase discovery terms, not workflow state.
- `depends_on`: card IDs that must be `done` before this card starts.
- `related`: useful cross-links that do not block work.

The frontmatter is the canonical current state. The dashboard below is a human-friendly projection and must be updated in the same change as a status or priority change.

## Ready

| ID | Priority | Task | Dependencies |
|---|---|---|---|
No ready cards.

## Backlog

| ID | Priority | Task | Dependencies |
|---|---|---|---|
No backlog cards.

## In Progress

| ID | Priority | Task | Owner | Dependencies |
|---|---|---|---|---|
No cards in progress.

## Blocked

No blocked cards.

## Done

| ID | Priority | Task | Completed |
|---|---|---|---|
| PAN-001 | P0 | [Persist workflow state](PAN-001-persist-workflow-state.md) | 2026-09-12 |
| PAN-002 | P0 | [Make delivery and mutations crash-safe](PAN-002-crash-safe-workflows.md) | 2026-09-14 |
| PAN-003 | P1 | [Complete the scheduler registry](PAN-003-scheduler-registry.md) | 2026-09-14 |
| PAN-004 | P1 | [Complete Fastmail triage](PAN-004-fastmail-triage.md) | 2026-09-14 |
| PAN-005 | P1 | [Harden and test Matrix](PAN-005-matrix-reliability.md) | 2026-09-14 |
| PAN-006 | P2 | [Complete CLI and HTTP interfaces](PAN-006-cli-http-interfaces.md) | 2026-09-14 |
| PAN-007 | P2 | [Decide and implement agent topology](PAN-007-agent-topology.md) | 2026-09-14 |

## Cancelled

No cancelled cards.
