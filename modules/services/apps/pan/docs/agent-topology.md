# Pan agent topology decision

Pan uses one general-purpose assistant for read-only questions and deterministic application
services for transaction review and email triage. The assistant owns the read-only finance and
mailbox tools; the Matrix and CLI workflow boundaries own all mutations.

| Tool group | Owner | Access |
|---|---|---|
| Finance queries | General assistant | Read |
| Fastmail mailbox and unread-email queries | General assistant | Read |
| Lunch Money review update | Transaction review service | Mutate, workflow confirmation required |
| Fastmail mailbox and seen update | Email triage service | Mutate, workflow confirmation required |

The alternatives considered were a routed set of specialist agents and a deterministic-only
system. Specialist routing adds prompts, hand-offs, and evaluation surface without improving the
two currently deterministic workflows. A deterministic-only system would remove useful ad-hoc
read-only assistance. Pan therefore keeps one assistant with a maximum of five tool turns.

The assistant never receives mutation tools, a confirmation token, or workflow action payloads.
Only a reply in the Matrix or CLI conversation that is bound to a durable pending workflow can
confirm a mutation. Conversation memory is ephemeral assistant context; workflow state is stored
separately in SQLite and is never inferred from model memory.

Calls have a 30-second timeout and are cancelled when the caller disconnects. Evaluation cases
cover read-only tool use, refusal of mutation requests, and explicit workflow confirmation.
