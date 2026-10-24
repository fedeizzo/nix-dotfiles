"""Toggle microphone meetings and export the completed, cleaned transcript."""

import fcntl
import os
from pathlib import Path
import re
import subprocess
import sys
import time
from datetime import datetime


def notify(message):
    subprocess.run(
        ["notify-send", "--app-name=Voxtype", "Voxtype meeting", message],
        check=False,
        timeout=10,
    )


def copy_actions(destination):
    try:
        result = subprocess.run(
            ["notify-send", "--app-name=Voxtype", "--wait", "--expire-time=60000",
             "--action=copy-path=Copy path", "--action=copy-content=Copy text",
             "Transcript saved", str(destination)],
            capture_output=True,
            text=True,
            check=True,
            timeout=65,
        )
        action = result.stdout.strip()
        if action == "copy-path":
            content = str(destination.resolve())
        elif action == "copy-content":
            content = destination.read_text()
        else:
            return  # Notification dismissed or expired.
        subprocess.run(
            ["wl-copy", "--type", "text/plain"],
            input=content,
            text=True,
            check=True,
            timeout=10,
        )
    except subprocess.TimeoutExpired:
        return
    except (subprocess.SubprocessError, OSError) as error:
        notify(f"Transcript saved: {destination}. Clipboard action failed: {error}")


def meeting(*args):
    result = subprocess.run(
        ["voxtype", "meeting", *args],
        capture_output=True,
        text=True,
        timeout=30,
    )
    if result.returncode:
        raise RuntimeError(result.stderr.strip() or result.stdout.strip())
    return result.stdout


def status():
    output = meeting("status")
    phase = re.search(r"^Meeting Status: (\w+)$", output, re.MULTILINE)
    identifier = re.search(r"^Meeting ID: ([\w-]+)$", output, re.MULTILINE)
    if phase:
        return phase[1], identifier[1] if identifier else None
    if "No meeting currently in progress." in output:
        return "idle", None
    raise RuntimeError(f"Unrecognized meeting status: {output.strip()}")


def wait_for(predicate, timeout=300):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        current = status()
        if predicate(current):
            return current
        time.sleep(0.5)
    raise RuntimeError("Timed out waiting for Voxtype. Check journalctl --user -u voxtype.")


def toggle(output_dir):
    phase, identifier = status()
    if phase == "idle":
        title = datetime.now().astimezone().strftime("Meeting %Y-%m-%d %H:%M:%S %Z")
        meeting("start", "--title", title)
        wait_for(lambda current: current[0] == "recording", timeout=180)
        notify("Recording started — microphone only. Press again to stop and save.")
    elif phase in {"recording", "paused"}:
        if not identifier:
            raise RuntimeError("Active meeting has no ID; refusing to export an unrelated meeting.")
        meeting("stop")
        notify("Stopping recording and finishing transcription…")
        wait_for(lambda current: current[0] == "idle")
        output_dir.mkdir(parents=True, exist_ok=True)
        # IDs uniquely identify sessions; export that exact meeting, never 'latest'.
        destination = output_dir / f"{identifier}.md"
        temporary = destination.with_suffix(".md.tmp")
        try:
            meeting("export", identifier, "--format", "markdown", "--timestamps",
                    "--metadata", "--output", str(temporary))
            temporary.replace(destination)
        finally:
            temporary.unlink(missing_ok=True)
        subprocess.Popen(
            [sys.executable, __file__, "--copy-actions", str(destination)],
            start_new_session=True,
            close_fds=True,
        )
    else:
        notify(f"Meeting is {phase}. Please wait before pressing again.")


def main():
    if sys.argv[1] == "--copy-actions":
        copy_actions(Path(sys.argv[2]))
        return
    runtime = Path(os.environ["XDG_RUNTIME_DIR"])
    with (runtime / "voxtype-meeting-toggle.lock").open("w") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            return
        toggle(Path(sys.argv[1]))


if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        print(f"Voxtype meeting failed: {error}", file=sys.stderr)
        notify(f"Meeting action failed: {error}. Saved meeting data remains in Voxtype storage.")
        sys.exit(1)
