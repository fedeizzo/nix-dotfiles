# Pan scheduler jobs

Each job has a unique `name`, a five-, six-, or seven-field cron `spec`, a `runner`, and a
matching `condition`. Supported pairs are `lunchmoney` with `lunchmoney:has_unreviewed`, and
`fastmail` with `fastmail:has_unread`. `mailbox` selects the Fastmail mailbox and defaults to
`Inbox`. `prompt` is retained for configuration compatibility and is ignored by deterministic
runners.

The condition is evaluated by the runner's durable `prepare` operation: it only delivers a card
when it can atomically reserve eligible work. This avoids a separate fetch before dispatch.
