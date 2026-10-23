# Pan

Pan is a personal assistant built in Rust, with Matrix and CLI interfaces. It connects to Fastmail and Lunch Money through read-only assistant tools and explicit confirmation workflows.

## Vision

Pan aims to be an everyday assistant you can talk to through text and, eventually, voice, with clear permissions and approval boundaries. It should use existing capabilities when available and help design missing ones through conversation: agree on the behavior and access needed, delegate implementation to an isolated Pi worker, and iterate together on the result. Development should preserve the conversation and workspace across follow-ups, with changes reviewed through PRs and explicitly approved before deployment. Pan can propose improvements to itself, while the user controls which changes and permissions become part of the running assistant.

## Current capabilities

| Capability | What it does |
|---|---|
| Finance questions | Reads accounts, categories, tags, and unreviewed Lunch Money transactions; calculates net worth. |
| Transaction review | Presents an unreviewed transaction and accepts confirmed category, tag, and note changes. |
| Email questions | Lists Fastmail mailboxes and retrieves unread email previews. |
| Email triage | Presents an unread email; confirmation can add a mailbox and mark the message as seen. |
| Scheduled reviews | Delivers transaction-review or email-triage cards to Matrix on configured schedules. |
| Recovery | Persists pending workflows in SQLite, restores confirmations after restarts, and prevents duplicate resource claims. |
| Development worker | Runs explicit development requests in isolated Pi jobs and delivers completion summaries and patches to Matrix. |

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

The homelab configuration enables Rust Pan and its development worker, using Qwen 27B (`qwen3.8-27b`) through llama-swap. The service is deployed on homelab, and a real Matrix development request has successfully produced a change and review artifacts.

## Development worker

The worker can run directly with `pan worker --worker-config <path>`, without
systemd. It requires its own configuration and sandbox launchers. See
[development worker documentation](docs/development-worker.md) for the setup,
status, cancellation, and current limits. PR publication is not implemented.

## Assistant timeout

Set `models.timeout_seconds` to a positive number of seconds. It covers the entire assistant request, including model loading, thinking, and tool calls. The default is 30 seconds; for local models, start with 180:

```yaml
models:
  name: qwen3.8-27b
  openai_base_url: https://llama.fedeizzo.dev/v1
  openai_api_key: placeholder
  timeout_seconds: 180
```

Restart Pan after changing its configuration.
