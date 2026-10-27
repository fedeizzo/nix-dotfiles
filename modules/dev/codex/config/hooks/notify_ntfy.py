#!/usr/bin/env python3
"""Send a concise, conversation-specific ntfy notification."""

import json
import sys
from pathlib import Path
from urllib.error import URLError
from urllib.request import Request, urlopen


NTFY_URL = "https://ntfy.fedeizzo.dev/agent"
MAX_PREVIEW_CHARS = 220


def notification_content(event: dict[str, object]) -> tuple[str, str]:
    cwd = event.get("cwd")
    project = Path(cwd).name if isinstance(cwd, str) and cwd else "session"
    title = f"Codex · {project}"

    last_message = event.get("last_assistant_message")
    if isinstance(last_message, str) and last_message.strip():
        preview = " ".join(last_message.split())
        if len(preview) > MAX_PREVIEW_CHARS:
            preview = preview[: MAX_PREVIEW_CHARS - 1].rstrip() + "…"
        message = f"{preview}\n\nWaiting for your input."
    else:
        message = "Codex is waiting for your input."
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
    print(json.dumps({"continue": True}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
