#!/usr/bin/env python3
"""Send a concise, conversation-specific ntfy notification."""

import json
import sys
from pathlib import Path
from urllib.error import URLError
from urllib.request import Request, urlopen


NTFY_URL = "https://ntfy.fedeizzo.dev/agent"
MAX_PREVIEW_CHARS = 220


def last_assistant_text(transcript_path: object) -> str:
    """Return the text of the last assistant message in the transcript."""
    if not isinstance(transcript_path, str) or not transcript_path:
        return ""
    try:
        lines = Path(transcript_path).read_text(encoding="utf-8").splitlines()
    except OSError:
        return ""
    for line in reversed(lines):
        try:
            entry = json.loads(line)
        except json.JSONDecodeError:
            continue
        if not isinstance(entry, dict) or entry.get("type") != "assistant":
            continue
        content = entry.get("message", {}).get("content")
        if isinstance(content, str):
            text = content
        elif isinstance(content, list):
            text = " ".join(
                block.get("text", "")
                for block in content
                if isinstance(block, dict) and block.get("type") == "text"
            )
        else:
            text = ""
        if text.strip():
            return text
    return ""


def notification_content(event: dict[str, object]) -> tuple[str, str]:
    cwd = event.get("cwd")
    project = Path(cwd).name if isinstance(cwd, str) and cwd else "session"
    title = f"Claude Code · {project}"

    last_message = event.get("last_assistant_message")
    if not isinstance(last_message, str) or not last_message.strip():
        last_message = last_assistant_text(event.get("transcript_path"))
    if last_message.strip():
        preview = " ".join(last_message.split())
        if len(preview) > MAX_PREVIEW_CHARS:
            preview = preview[: MAX_PREVIEW_CHARS - 1].rstrip() + "…"
        message = f"{preview}\n\nWaiting for your input."
    else:
        message = "Claude Code is waiting for your input."
    return title, message


def main() -> int:
    try:
        event = json.load(sys.stdin)
    except (json.JSONDecodeError, TypeError):
        event = {}
    if not isinstance(event, dict):
        event = {}

    title, message = notification_content(event)
    request = Request(
        NTFY_URL,
        data=message.encode("utf-8"),
        headers={
            "Content-Type": "text/plain; charset=utf-8",
            "Title": title,
        },
        method="POST",
    )
    try:
        with urlopen(request, timeout=4) as response:
            response.read()
    except (OSError, URLError) as error:
        print(f"ntfy notification failed: {error}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
