# Pan

Pan is a personal assistant built in Rust, with Matrix and CLI interfaces. It connects to Fastmail and Lunch Money through read-only assistant tools and explicit confirmation workflows.

## Current capabilities

| Capability | What it does |
|---|---|
| Finance questions | Reads accounts, categories, tags, and unreviewed Lunch Money transactions; calculates net worth. |
| Transaction review | Presents an unreviewed transaction and accepts confirmed category, tag, and note changes. |
| Email questions | Lists Fastmail mailboxes and retrieves unread email previews. |
| Email triage | Presents an unread email; confirmation can add a mailbox and mark the message as seen. |
| Scheduled reviews | Delivers transaction-review or email-triage cards to Matrix on configured schedules. |
| Recovery | Persists pending workflows in SQLite, restores confirmations after restarts, and prevents duplicate resource claims. |

The model receives only read tools. Application workflows require explicit confirmation before changing email or transaction data. Email triage suggestions currently only propose marking the message as seen.

## CLI

```sh
# Start interactive assistant chat with a CLI interface configuration
pan --config pan.yaml

# Preview the next transaction without changing it
pan --config pan.yaml review-transaction
pan --config pan.yaml review-transaction --json

# Confirm review of a specific transaction using its current values
pan --config pan.yaml apply-transaction 42 --confirm
```

The standalone transaction commands require only Lunch Money connectivity. See [CLI documentation](docs/cli.md), [scheduler configuration](docs/scheduler.md), and [agent topology](docs/agent-topology.md).

## Current limits

Pan does not yet manage GitHub issues or PRs, monitor machines, install updates, send email, or generate dedicated spending reports. There is no connected browser interface.

The Rust service is currently disabled in its bundled NixOS configuration; Framework Desktop still imports the older Go Pan module.
