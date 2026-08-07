#!/usr/bin/env bash

set -euo pipefail

host="bits@workspace-federico-izzo"

ssh -o BatchMode=yes "$host" 'bash -s' <<'REMOTE'
set -euo pipefail

if [[ "$(uname -s)" != "Linux" || "$(uname -m)" != "aarch64" ]]; then
  echo "Expected an aarch64 Linux workspace; found $(uname -s)/$(uname -m)." >&2
  exit 1
fi

if [[ "$(id -un)" != "bits" ]]; then
  echo "Expected to bootstrap the bits user; connected as $(id -un)." >&2
  exit 1
fi

if [[ ! -x "$HOME/.nix-profile/bin/nix" ]]; then
  command -v curl >/dev/null
  sudo -n true

  sudo mkdir -p /nix
  sudo chown "$(id -u):$(id -g)" /nix

  curl --proto '=https' --tlsv1.2 --fail --silent --show-error --location \
    https://nixos.org/nix/install | sh -s -- --no-daemon
fi

# deploy-rs executes Nix commands through a non-interactive SSH shell. The
# workspace's zsh startup does not add the single-user profile in that mode,
# so expose only the Nix command shims through its existing system PATH.
for command in nix nix-build nix-channel nix-collect-garbage nix-copy-closure \
  nix-daemon nix-env nix-hash nix-instantiate nix-prefetch-url nix-shell nix-store; do
  if [[ -x "$HOME/.nix-profile/bin/$command" ]]; then
    sudo ln -sfn "$HOME/.nix-profile/bin/$command" "/usr/local/bin/$command"
  fi
done

mkdir -p "$HOME/.config/nix"
touch "$HOME/.config/nix/nix.conf"
if ! grep -Eq '^experimental-features[[:space:]]*=' "$HOME/.config/nix/nix.conf"; then
  printf '\nexperimental-features = nix-command flakes\n' >> "$HOME/.config/nix/nix.conf"
fi

nix --version
nix config show experimental-features
REMOTE

# Probe the exact non-multiplexed ssh-ng transport deploy-rs will use before
# starting the potentially long remote build.
NIX_SSHOPTS='-o ControlMaster=no -o ControlPath=none -o RequestTTY=no' \
  nix store info --store "ssh-ng://$host"

if [[ "${1:-}" != "--bootstrap-only" ]]; then
  echo "The first full-profile remote build can be silent and take 30+ minutes; do not interrupt it." >&2
  if command -v deploy >/dev/null; then
    deploy --skip-checks '.#datadog-workspace'
  else
    nix develop --command deploy --skip-checks '.#datadog-workspace'
  fi
fi
