# Builds the jailed pi-coding-agent derivation using jail.nix
{ pkgs
, username
, package
, jail
, persistName ? "pi"
, allowNetwork ? true
,
}:

let
  pi-coding-agent = package;
  inherit (pkgs) lib;
  c = jail.combinators;
in
# jail "pi" pi-coding-agent (with c; [
jail "pi" pi-coding-agent (with c; [
  # Persistent cache/config directory
  (persist-home persistName)

  # Desktop notifications via D-Bus (allows talking to org.freedesktop.Notifications)
  (notifications)

  # Set hostname for isolation
  (set-hostname "pi-jail")

  # Tools available inside the jail
  (add-pkg-deps (with pkgs; [
    gnugrep
    findutils
    git
    fd
    ripgrep
    nodejs
    python3
    jj
  ] ++ [
    pi-coding-agent
  ]))

  # Forward the user's locale settings for proper text rendering
  (try-fwd-env "LANG")
  (try-fwd-env "LC_ALL")

  (readwrite "/home/${username}/.pi")

  # npm fixes
  (write-text (noescape "~/.npmrc") ''prefix = ''${HOME}/.npm-packages'')
  (add-path "~/.npm-packages/bin")
  (set-env "NODE_PATH" (noescape "~/.npm-packages/lib/node_modules"))

  (add-runtime ''
    RUNTIME_ARGS+=(--bind "''${PWD}" "/home/${username}/project")
    echo "📥 syncing pi config into jail..."
    if ! RSYNC_OUTPUT=$(${pkgs.rsync}/bin/rsync -Pavzh /home/${username}/nix-dotfiles/home/common/pi/config/agent /home/${username}/.pi 2>&1); then
      echo "❌ rsync failed (runtime):"
      echo "''${RSYNC_OUTPUT}"
      exit 1
    fi
    echo "✅ pi config synced!"
  '')
  (add-cleanup ''
    echo "📤 syncing pi config back out of jail..."
    if ! RSYNC_OUTPUT=$(${pkgs.rsync}/bin/rsync -Pavzh /home/${username}/.pi/agent/skills /home/${username}/nix-dotfiles/home/common/pi/config/agent 2>&1); then
      echo "❌ rsync failed (cleanup - skills):"
      echo "''${RSYNC_OUTPUT}"
      exit 1
    fi
    if ! RSYNC_OUTPUT=$(${pkgs.rsync}/bin/rsync -Pavzh /home/${username}/.pi/agent/models.json /home/${username}/nix-dotfiles/home/common/pi/config/agent 2>&1); then
      echo "❌ rsync failed (cleanup - models):"
      echo "''${RSYNC_OUTPUT}"
      exit 1
    fi
    if ! RSYNC_OUTPUT=$(${pkgs.rsync}/bin/rsync -Pavzh /home/${username}/.pi/agent/caveman.json /home/${username}/nix-dotfiles/home/common/pi/config/agent 2>&1); then
      echo "❌ rsync failed (cleanup - caveman):"
      echo "''${RSYNC_OUTPUT}"
      exit 1
    fi
    if ! RSYNC_OUTPUT=$(${pkgs.rsync}/bin/rsync -Pavzh /home/${username}/.pi/agent/settings.json /home/${username}/nix-dotfiles/home/common/pi/config/agent 2>&1); then
      echo "❌ rsync failed (cleanup - settings):"
      echo "''${RSYNC_OUTPUT}"
      exit 1
    fi
    echo "✅ pi config synced back!"
  '')

  (wrap-entry (entry: ''
    cd /home/${username}/project
    ${entry}
  ''))


  # Mount $PWD as the project workspace, must be done after persist-home otherwise it conflicts
  # mount-cwd
] ++ (lib.optional allowNetwork network))
