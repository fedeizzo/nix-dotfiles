# Jailed Pi Coding Agent

## Goal

A home-manager module under `@home/common/pi/` that wraps the `pi-coding-agent`
in a [jail.nix](https://sr.ht/~alexdavid/jail.nix) sandbox so Pi runs inside
a restricted environment. This enables **auto-approving all actions** without
fear of the agent damaging the host system.

## Constraints

| Constraint | Decision |
|---|---|
| Platforms | Linux only (bubblewrap is Linux-only) |
| Git credentials | None — no SSH agent forwarding, no git remotes that push to secrets |
| Shell tools | `git` (read-only ops), `ripgrep`, `nodejs`, `python3` |
| Notifications | Yes — D-Bus for desktop notifications on task completion |
| Working directory | `$PWD` — wherever the user runs the wrapper from |

## Module Structure

```
home/common/pi/
├── default.nix          # Home-manager module (config options + module body)
└── jail-wrapper.nix     # The jailed pi derivation (calls jail.nix)
```

## Flakes Integration

Add `jail.nix` as a flake input:

```nix
jail-nix.url = "sourcehut:~alexdavid/jail.nix";
```

The module is only imported on Linux hosts (via `lib.mkIf (pkgs.stdenv.isLinux)`).

## Jail Permissions

The jail uses `bubblewrap` via `jail.nix` combinators. Only the bare minimum
permissions are granted:

| Combinator | Purpose |
|---|---|
| `network` | LLM API calls, plugin downloads (opt-in via `allowNetwork`, default `true`) |
| `mount-cwd` | Read/write access to `$PWD` (the project workspace) |
| `persist-home "pi"` | Persistent cache/config in `~/.local/share/jail.nix/home/pi` |
| `dbus` | Desktop notifications when a task completes |
| `set-env` | Forward locale settings (`LANG`, `LC_ALL`) |
| `add-path` | Prepend jailed tools (`git`, `rg`, `node`, `python3`) to `$PATH` |
| `set-hostname` | Set hostname to `pi-jail` for isolation |

### Network restriction note
`bubblewrap` does not support domain-level DNS filtering. When `allowNetwork = true`,
the jail shares the full host network namespace. To restrict to only `https://llama.fedeizzo.dev`:
- Set `allowNetwork = false` and use a local model server
- Or run a custom DNS forwarder that only resolves the allowed domain (not implemented)

## What the Jail Blocks (by design)

- **No access** to real `~/.config`, `~/.ssh`, `/etc` (except nix store)
- **No write access** to host filesystem outside `$PWD`
- **No network tools** other than what Pi itself uses (API calls only)
- **No access** to `/var`, `/run` (except nix store runtime closure)
- **`/tmp` as tmpfs** — no persistent temp files leaking to host
- **No SSH agent** — cannot push/pull from remote repos with credentials

## Home-Manager Config Options

```nix
programs.jail-pi = {
  enable = true;

  # Persistent home name (stored in ~/.local/share/jail.nix/home/<name>)
  persistName = "pi";

  # Allow network access for LLM API calls
  allowNetwork = true;  # set to false for maximum isolation
};
```

## Output

The module produces a `pi-jail` binary in the user's `$PATH`. When invoked:

1. Bubblewrap is launched with the configured sandbox
2. `$PWD` is mounted as the project workspace inside the jail
3. `pi-coding-agent` runs inside the sandbox
4. Persistent state lives in `~/.local/share/jail.nix/home/pi`

The user calls `pi-jail` instead of `pi` to get the sandboxed experience.

## Implementation Steps

1. Add `jail-nix` to `flake.nix` inputs
2. Create `home/common/pi/jail-wrapper.nix` — the jailed derivation
3. Create `home/common/pi/default.nix` — the home-manager module
4. Add `../common/pi` to host imports (Linux hosts only)
