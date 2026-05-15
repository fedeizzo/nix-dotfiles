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
  entrypoint = pkgs.writeShellScriptBin "pi" ''
    if [ -z $1 ]; then
        ${pi-coding-agent}/bin/pi
    else
        eval $@
    fi
  '';
in
# jail "pi" pi-coding-agent (with c; [
jail "pi" entrypoint (with c; [
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
    nix
    direnv
    go
    gcc
    curl
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
  (add-path "~/go/bin")
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
    _SYNC_PAIRS=(
      ".pi/agent/skills:nix-dotfiles/home/common/pi/config/agent"
      ".pi/agent/extensions:nix-dotfiles/home/common/pi/config/agent"
      ".pi/agent/models.json:nix-dotfiles/home/common/pi/config/agent"
      ".pi/agent/caveman.json:nix-dotfiles/home/common/pi/config/agent"
      ".pi/agent/settings.json:nix-dotfiles/home/common/pi/config/agent"
      ".pi/piforge-self.md:nix-dotfiles/home/common/pi/config"
      ".pi/piforge.json:nix-dotfiles/home/common/pi/config"
    )
    for pair in "''${_SYNC_PAIRS[@]}"; do
      src="''${pair%%:*}"
      dst="''${pair##*:}"
      label="''${src##*/}"
      if ! RSYNC_OUTPUT=$(${pkgs.rsync}/bin/rsync -Pavzh "/home/${username}/''${src}" "/home/${username}/''${dst}" 2>&1); then
        echo "❌ rsync failed (cleanup - ''${label}):"
        echo "''${RSYNC_OUTPUT}"
        exit 1
      fi
    done
    echo "✅ pi config synced back!"
  '')

  (wrap-entry (entry: ''
    cd /home/${username}/project
    ${entry}
  ''))


  # Mount $PWD as the project workspace, must be done after persist-home otherwise it conflicts
  # mount-cwd
] ++ (lib.optional allowNetwork network))
