# Development worker

One Pan executable has two independent entry points: `pan serve` runs the assistant,
and `pan worker --worker-config <path>` runs the development queue. Worker and
`pan dev` commands do not load the assistant configuration or integration secrets.

## Local testing on Linux

Build Pan from the **nix-dotfiles repository root**, then start the worker directly:

```sh
cargo build --manifest-path modules/services/apps/pan/Cargo.toml --locked
./modules/services/apps/pan/target/debug/pan worker \
  --worker-config /tmp/pan-worker.yaml
```

The worker configuration is separate from the assistant's `config.yaml`. Supply
absolute paths for `socket`, `state_dir`, `sandbox_program`, `fetch_program`,
`pi_program`, `git_program`, and `jj_program`, plus the tool `path`, positive
`timeout_seconds`, and a `repositories` allowlist containing HTTPS `url` and
`branch` values. The sandbox/fetch launchers must be prepared before running it;
this is not a turnkey local setup.

No systemd service or NixOS activation is required. Local testing requires
manually prepared launchers and a model bridge; there is no bundled local test
launcher.

In another terminal, use the socket from the worker configuration:

```sh
PAN_SOCKET=/tmp/pan-dev-EXAMPLE/control.sock
PAN_BIN=./modules/services/apps/pan/target/debug/pan

"$PAN_BIN" dev --socket "$PAN_SOCKET" submit pan \
  'Add a brief description of the development worker to the Pan README.'

# Use the ID returned by submit:
"$PAN_BIN" dev --socket "$PAN_SOCKET" status JOB_ID
"$PAN_BIN" dev --socket "$PAN_SOCKET" cancel JOB_ID
```

Set the public HTTPS test repository and branch in the worker's allowlist before
starting it. Requests cannot supply arbitrary repository URLs or executable paths.

Ctrl-C stops the local worker. Artifacts remain under the configured state directory:

```text
jobs.sqlite3
jobs/<job-id>/workspace/     # disposable clone with a colocated jj repository
jobs/<job-id>/changes.patch
jobs/<job-id>/summary.md
```

The laptop's working copy is never passed into Pi. Only its disposable clone is
writable inside the sandbox. The local worker supervisor runs as your user, so
local testing exercises the coding sandbox but does not reproduce the service's
additional Unix-user and systemd boundaries.

## Matrix

When enabled in the Rust assistant, use:

```text
dev: pan Add feature X, with acceptance criteria Y.
dev-status <job-id>
dev-cancel <job-id>
```

Commands require both the configured Matrix sender and room. Job submissions use
the Matrix event ID for deduplication and retain sender, room, and thread binding.
Responses stay in the originating thread. Pan polls for terminal jobs every five
seconds and sends the completion/failure summary plus a patch attachment for
completed jobs with changes. Delivery is persisted until acknowledged, and stable
Matrix transaction IDs prevent duplicate result events on retry. Status and
cancellation remain available explicitly; streaming progress and follow-up prompts
are not implemented yet. Development requests bypass the assistant's model entirely.

## NixOS

Import `apps-pan` and its `apps-pan-config` configuration, then enable the Rust
assistant. The bundled configuration enables the worker alongside it:

```nix
services.apps-pan = {
  enable = true;
  development = {
    enable = true;
    timeoutSeconds = 1800;
    repositories.pan = {
      url = "https://github.com/fedeizzo/nix-dotfiles.git";
      branch = "main";
    };
  };
};
```

The homelab imports Rust Pan and enables both services. The assistant and Pi use
Qwen 27B (`qwen3.8-27b`); assistant requests have a 180-second deadline. The
assistant uses `/var/lib/pan`, reusing the old Go service's directory with fresh
state. Secrets are owned by `pan-rust`. The old Go service and its RSS schedule
are no longer enabled.

### First deployment and verification

On your laptop, from the nix-dotfiles root, build the actual homelab configuration:

```sh
nix build .#nixosConfigurations.homelab.config.system.build.toplevel
```

Before activating it, run these commands **on homelab** to discard the old Go
state, as requested. The second command deletes everything inside Pan's state
directory, including its persisted contents:

```sh
sudo systemctl stop pan.service
sudo find /var/lib/pan -xdev -mindepth 1 -delete
```

Then deploy from your laptop using the existing deploy-rs workflow:

```sh
deploy --skip-checks .#homelab
```

On homelab, verify service startup and the local model bridge:

```sh
systemctl status apps-pan.service pan-dev.service pan-dev-model.socket
journalctl -u apps-pan.service -u pan-dev.service -u pan-dev-model.service -n 80 --no-pager
sudo tail -n 50 /var/lib/pan/pan.log
curl -fsS http://127.0.0.1:11435/v1/models
```

The model bridge service starts on the first connection; its socket must be
listening beforehand. Fresh Matrix state creates a new login/device; verify it
in your Matrix client if requested. In the configured room, first send a simple
chat message, then:

```text
dev: pan Add a short paragraph to modules/services/apps/pan/README.md explaining how to check and cancel a development job. Only change documentation.
```

Expect a queued job ID, a completion or failure reply in the same thread, and a
`.patch` attachment when files changed. Check the worker journal if it fails. This
documentation task does not require dependency downloads. For cancellation,
submit another job and send `dev-cancel <job-id>`; expect a cancelled result.

The worker clones the configured remote branch, so it sees the version published
there, independently of the currently deployed Pan binary.

Systemd runs `apps-pan` as `pan-rust`, and `pan-dev` as `pan-dev`. Only the
`pan-dev-client` group can reach `/run/pan-dev/control.sock`. Worker state lives in
`/var/lib/pan-dev`, is persisted through the homelab's persistence mechanism, and
is private to the worker. The assistant communicates through the socket and does
not need access to that directory.

The worker has an 8 GiB memory limit, two CPUs of quota, 256 tasks, and one active
job. At most 20 jobs may be queued/running. The configured timeout covers cloning,
Pi, and artifact collection. Cancellation drops the sandbox process; its PID
namespace and `--die-with-parent` remove the child process tree. Systemd also uses
`KillMode=control-group` on service shutdown. Queued jobs survive restart; running
jobs become `interrupted` and require a new explicit request.

## Pi configuration and enforced boundaries

The shared launcher definition is in `worker.n.nix`. Pi receives immutable,
Pan-owned settings, model configuration, and instructions from the Nix store.
`worker-instructions.md` supplies the default guidance, overridable through
`services.apps-pan.development.instructions`. No personal Pi directory is copied,
imported, or linked. Startup disables project trust, extensions, MCP, skills,
templates, themes, context discovery, telemetry, and startup network operations.
The launcher explicitly selects `pan-local/qwen3.8-27b`.

Bubblewrap exposes only `/nix/store` read-only, a fresh `/proc`, minimal devices,
temporary directories, the job checkout, and the model bridge socket. It gives Pi
a separate PID/network namespace, no capabilities, no host home, no host runtime
sockets, and no access to the worker database or other jobs. Model configuration
and instructions cannot be overwritten. Repository instructions remain advisory;
filesystem/process/network restrictions are enforced outside the model.

The preparation sandbox has network access only while running the trusted clone
command, with no agent, inherited credentials, or host configuration. The coding
sandbox has no host network. Its only network route is a local relay to the model
server. The service bridge reaches local llama-swap. Code and prompts are sent to
that endpoint.

## Current limits

- Jobs produce patches and a model-written check summary. These check claims are
  review evidence, not independent validation by Pan.
- There are no GitHub credentials, publication, PR creation, trusted signing,
  merging, deployment, or access to the Nix daemon.
- Dependency downloads and internet-dependent tests cannot run in the coding
  sandbox. The initial toolchain includes Rust, GCC, Git/Jujutsu, and Nix tools;
  dependencies need a future controlled build/cache integration.
- Resource limits apply to the worker service. Local tests use the sandbox
  boundaries but do not install systemd resource limits.
- Artifacts are retained until the operator removes the stopped worker's job
  directory. Automated retention, disk quotas, richer progress, follow-ups, and
  NixOS VM isolation tests remain tracked in PAN-008.

Pi RPC completion waits for `agent_settled`, not prompt acceptance or `agent_end`.
See the [Pi RPC reference](https://pi.dev/docs/latest/rpc). Malformed/oversized
records, provider errors, and unexpected exits fail the job. Diagnostics are
drained without retaining raw prompts, tool output, or provider credentials.
