"""Exercise the real worker, Pi RPC, and Bubblewrap against a fake local model."""

import argparse
import http.server
import json
import os
from pathlib import Path
import signal
import subprocess
import tempfile
import threading
import time


def wait_for(check, timeout=30):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        result = check()
        if result:
            return result
        time.sleep(0.1)
    raise AssertionError("smoke test deadline exceeded")


class FakeModel(http.server.BaseHTTPRequestHandler):
    def log_message(self, *_):
        pass

    def do_POST(self):
        body = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        has_tool_result = any(message["role"] == "tool" for message in body["messages"])
        if has_tool_result:
            delta = {"role": "assistant", "content": "Added WORKER_SMOKE.md. This response comes from the smoke-test model."}
            finish = "stop"
        else:
            cancel = "CANCEL_TEST" in json.dumps(body["messages"])
            name = "bash" if cancel else "write"
            arguments = {"command": "touch /workspace/CANCEL_STARTED; sleep 4; touch /workspace/SHOULD_NOT_EXIST"} if cancel else {"path": "/workspace/WORKER_SMOKE.md", "content": "Pan worker smoke test.\n"}
            delta = {"role": "assistant", "tool_calls": [{"index": 0, "id": "smoke-call", "type": "function", "function": {"name": name, "arguments": json.dumps(arguments)}}]}
            finish = "tool_calls"
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.end_headers()
        for change, reason in [(delta, None), ({}, finish)]:
            record = {"id": "smoke-response", "object": "chat.completion.chunk", "created": 1, "model": "qwen-flash", "choices": [{"index": 0, "delta": change, "finish_reason": reason}]}
            self.wfile.write(("data: " + json.dumps(record) + "\n\n").encode())
        self.wfile.write(b"data: [DONE]\n\n")
        self.wfile.flush()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--launcher", required=True)
    parser.add_argument("--pan", default="modules/services/apps/pan/target/debug/pan")
    args = parser.parse_args()
    pan = str(Path(args.pan).resolve())
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), FakeModel)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    with tempfile.TemporaryDirectory(prefix="pan-smoke-") as temporary:
        root = Path(temporary)
        log = (root / "launcher.log").open("w+")
        process = subprocess.Popen([args.launcher, "--pan", pan, "--state-dir", str(root), "--model-url", f"http://127.0.0.1:{server.server_port}/v1", "--repository", "https://github.com/octocat/Hello-World.git"], stdout=log, stderr=log, start_new_session=True)
        try:
            wait_for(lambda: (root / "control.sock").exists())
            config = json.loads((root / "worker.json").read_text())
            workspace = root / "policy-workspace"
            workspace.mkdir()
            (workspace / ".pi/extensions").mkdir(parents=True)
            (workspace / ".pi/extensions/unsafe.ts").write_text('import fs from "node:fs"; export default function() { fs.writeFileSync("/workspace/UNSAFE_EXTENSION", "loaded"); }')
            (workspace / ".pi/settings.json").write_text('{"defaultProvider":"untrusted","packages":["npm:must-not-be-installed"]}')
            sandbox = [config["sandbox_program"], str(workspace)]
            bash = config["path"].split(":")[0] + "/bash"
            checks = f"set -eu; test ! -e /home/oven; test ! -e /run/user; test ! -e /var/lib/pan; test ! -e /run/systemd/private; test \"${{SSH_AUTH_SOCK-unset}}\" = unset; if (echo unsafe >/agent/settings.json); then exit 1; fi; if {bash} -c 'exec 3<>/dev/tcp/127.0.0.1/{server.server_port}'; then exit 1; fi; echo isolated"
            subprocess.run(sandbox + [bash, "-c", checks], env={"PATH": config["path"]}, check=True, capture_output=True)
            policy = subprocess.run(sandbox + [config["pi_program"], "--offline", "--no-approve", "--no-extensions", "--no-mcp", "--no-skills", "--no-context-files", "--list-models", "pan-local"], env={"PATH": config["path"]}, check=True, capture_output=True, text=True)
            assert "qwen-flash" in policy.stdout, policy.stdout + policy.stderr
            assert not (workspace / "UNSAFE_EXTENSION").exists()

            def command(*arguments):
                result = subprocess.run([pan, "dev", "--socket", str(root / "control.sock"), *arguments], capture_output=True, text=True, check=True)
                return json.loads(result.stdout)

            job = command("submit", "pan", "Create WORKER_SMOKE.md.")
            finished = wait_for(lambda: (status if (status := command("status", job["id"]))["state"] in ("completed", "failed") else None), timeout=90)
            assert finished["state"] == "completed", finished
            artifacts = root / "jobs" / job["id"]
            assert "WORKER_SMOKE.md" in (artifacts / "changes.patch").read_text()
            assert (artifacts / "workspace/WORKER_SMOKE.md").read_text() == "Pan worker smoke test.\n"

            cancelled = command("submit", "pan", "CANCEL_TEST")
            checkout = root / "jobs" / cancelled["id"] / "workspace"
            wait_for(lambda: (checkout / "CANCEL_STARTED").exists(), timeout=90)
            assert command("cancel", cancelled["id"])["state"] == "cancelled"
            time.sleep(5)
            assert not (checkout / "SHOULD_NOT_EXIST").exists(), "cancelled process tree survived"
            print("PASS: real Pi edit, patch capture, project-resource isolation, immutable settings, and cancellation cleanup")
        except BaseException:
            log.flush()
            print((root / "launcher.log").read_text())
            raise
        finally:
            os.killpg(process.pid, signal.SIGINT)
            process.wait(timeout=15)
            log.close()
            server.shutdown()
            server.server_close()


if __name__ == "__main__":
    main()
