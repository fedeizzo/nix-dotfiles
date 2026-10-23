"""Run the real Pan worker and sandbox locally, with no systemd or personal secrets."""

import argparse
import http.client
import http.server
import json
import os
from pathlib import Path
import shlex
import signal
import socketserver
import ssl
import subprocess
import tempfile
import threading
from urllib.parse import urlsplit


class ModelBridge(socketserver.ThreadingMixIn, socketserver.UnixStreamServer):
    daemon_threads = True


def main():
    def interrupted(*_):
        raise KeyboardInterrupt

    signal.signal(signal.SIGTERM, interrupted)
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pan", default="modules/services/apps/pan/target/debug/pan")
    parser.add_argument("--state-dir", type=Path)
    parser.add_argument("--model-url", default="https://llama.fedeizzo.dev/v1")
    parser.add_argument("--repository", default="https://github.com/fedeizzo/nix-dotfiles.git")
    parser.add_argument("--branch", default="master")
    for name in ("sandbox", "fetch", "pi", "git", "jj", "tool-path"):
        parser.add_argument(f"--{name}", required=True)
    args = parser.parse_args()
    pan = Path(args.pan).resolve()
    if not pan.is_file():
        parser.error("build Pan first: cargo build --manifest-path modules/services/apps/pan/Cargo.toml --locked")
    endpoint = urlsplit(args.model_url)
    if endpoint.scheme not in ("http", "https") or not endpoint.hostname or endpoint.username:
        parser.error("--model-url must be an HTTP(S) endpoint without credentials")
    state = (args.state_dir or Path(tempfile.mkdtemp(prefix="pan-dev-"))).resolve()
    state.mkdir(mode=0o700, parents=True, exist_ok=True)
    os.chmod(state, 0o700)
    model_socket = state / "model.sock"

    class Handler(http.server.BaseHTTPRequestHandler):
        def log_message(self, *_):
            pass

        def do_POST(self):
            # Expose only the model's completions endpoint, not other host services.
            if self.path != "/v1/chat/completions":
                self.send_error(404)
                return
            connection = None
            try:
                length = int(self.headers.get("Content-Length", "0"))
                if not 0 < length <= 16 * 1024 * 1024:
                    self.send_error(413)
                    return
                if endpoint.scheme == "https":
                    connection = http.client.HTTPSConnection(endpoint.hostname, endpoint.port, timeout=600, context=ssl.create_default_context())
                else:
                    connection = http.client.HTTPConnection(endpoint.hostname, endpoint.port, timeout=600)
                connection.request("POST", endpoint.path.rstrip("/") + "/chat/completions", self.rfile.read(length), {"Content-Type": "application/json", "Authorization": "Bearer placeholder"})
                response = connection.getresponse()
                self.send_response(response.status)
                self.send_header("Content-Type", response.getheader("Content-Type", "application/json"))
                self.send_header("Connection", "close")
                self.end_headers()
                while chunk := response.read1(65536):
                    self.wfile.write(chunk)
                    self.wfile.flush()
            except (OSError, ValueError, http.client.HTTPException):
                self.close_connection = True
            finally:
                if connection:
                    connection.close()

    # Never unlink a potentially active bridge from another local launcher.
    bridge = ModelBridge(str(model_socket), Handler)
    os.chmod(model_socket, 0o600)
    threading.Thread(target=bridge.serve_forever, daemon=True).start()
    sandbox = state / "sandbox"
    sandbox.write_text("#!/bin/sh\nexec env " + shlex.quote("PAN_DEV_MODEL_SOCKET=" + str(model_socket)) + " " + shlex.quote(args.sandbox) + ' "$@"\n')
    sandbox.chmod(0o700)
    configuration = {
        "socket": str(state / "control.sock"), "state_dir": str(state),
        "sandbox_program": str(sandbox), "fetch_program": args.fetch,
        "pi_program": args.pi, "git_program": args.git, "jj_program": args.jj,
        "path": args.tool_path, "timeout_seconds": 1800,
        "repositories": {"pan": {"url": args.repository, "branch": args.branch}},
    }
    config = state / "worker.json"
    config.write_text(json.dumps(configuration, indent=2) + "\n")
    print(f"Worker state: {state}\nModel endpoint: {args.model_url}", flush=True)
    print("In another terminal:\n" + shlex.join([str(pan), "dev", "--socket", configuration["socket"], "submit", "pan", "Add a short comment to the README describing Pan."]), flush=True)
    child = None
    try:
        # Keep the worker itself free of inherited provider/SSH/GitHub credentials.
        child = subprocess.Popen([str(pan), "worker", "--worker-config", str(config)], env={"PATH": args.tool_path, "LANG": "C.UTF-8"})
        return child.wait()
    except KeyboardInterrupt:
        if child and child.poll() is None:
            child.terminate()
            child.wait(timeout=10)
        return 130
    finally:
        bridge.shutdown()
        bridge.server_close()
        model_socket.unlink(missing_ok=True)


if __name__ == "__main__":
    raise SystemExit(main())
