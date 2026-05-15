---
name: local-review
description: Searches for TODO(pi) comments via ripgrep, generates a todo list, and addresses each item iteratively. Use when user invokes /local-review or asks to process code review comments.
---

# Local Review Skill

Process `TODO(pi)` annotations left in code as human review feedback.

## Trigger
- User types `/local-review`
- User says "address review comments" / "handle TODOs"
- No TODO(pi) found → report and halt

## Step 1 — Scan

Run ripgrep in project root:
```bash
rg -n --no-heading 'TODO\(pi\)' .
```

Parse format: `filename:line_number:content TODO(pi) message`
- `file`: path relative to project root
- `line`: line number
- `message`: text after `TODO(pi)`, trimmed
- Multiple TODO(pi) per line → separate items
- Empty message → use surrounding context
- Binary/excluded dirs → skipped by `rg` defaults

Zero results → inform user, halt. Otherwise proceed to Step 2.

## Step 2 — Address Items Iteratively

**Create todo list.** For each item from Step 1, create a todo:
- `subject`: first ~50 chars of message (truncate with `...`)
- `description`: full message + file path + line number
- `metadata.file`: parsed file path
- `metadata.line`: parsed line number

**Iterate.** For each todo (in order):
1. Mark `in_progress` with `activeForm`
2. `read` the file, `limit` covering `metadata.line` ± 10 lines
3. Implement fix based on TODO message
4. Strip `TODO(pi) ...` from the line
5. Re-read affected lines, confirm correct
6. Mark `completed`

**Skip rules.** Unclear message → skip with reason in metadata. Architecture decision needed → skip, ask user via `ask_user_question`. Max 5 iterations per turn. If more remain, stop and report progress.

## Step 3 — Report

Run ripgrep again — confirm zero `TODO(pi)` remain.

Print summary:
- Total items found
- Items addressed successfully
- Items skipped (with reason)
- Any remaining TODO(pi) locations

Clear todo list via `todo clear` if all done.
