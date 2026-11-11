{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";
  flake-file.inputs.herdr-app-jj = {
    url = "github:sebcbi1/herdr-app-jj";
    flake = false;
  };
  flake-file.inputs.herdr-kanso = {
    url = "github:jlopezlira/herdr-kanso";
    flake = false;
  };
  flake-file.inputs.herdr-usage-limits = {
    url = "github:nozikov/herdr-usage-limits";
    flake = false;
  };
  flake-file.inputs.herdr-mods = {
    url = "github:cybertron288/herdr-mods";
    flake = false;
  };

  # The Collie panel and bridge are a separate import: hosts that only want the
  # app (work laptop) take herdr-app alone. herdr-app already wraps herdr-core
  # into its PATH, so nothing here depends on Collie.
  flake.modules.homeManager.herdr-app-collie = { inputs, config, ... }: {
    imports = [ inputs.self.modules.homeManager.collie ];
    xdg.configFile."herdr-app/mods/collie".source = config.fi.collie.modSource;
  };

  flake.modules.homeManager.herdr-app = { pkgs, lib, inputs, config, ... }:
  let
    cfg = config.fi.herdr-app.usageLimits;
    # Upstream hardcodes one Claude and one Codex provider; install-mods.py
    # turns each account into its own provider (see there).
    accountsJson = pkgs.writeText "usage-limits-accounts.json" (builtins.toJSON cfg.accounts);
  in {
    options.fi.herdr-app.usageLimits.accounts = lib.mkOption {
      description = ''
        Subscriptions shown by the usage-limits mod, in status-bar order. `home`
        is the account's CLAUDE_CONFIG_DIR / CODEX_HOME (null = CLI default);
        antigravity accounts must leave it null.
      '';
      type = lib.types.listOf (lib.types.submodule {
        options = {
          id = lib.mkOption { type = lib.types.strMatching "[a-z0-9-]+"; };
          name = lib.mkOption { type = lib.types.str; };
          provider = lib.mkOption { type = lib.types.enum [ "claude" "codex" "antigravity" ]; };
          home = lib.mkOption { type = lib.types.nullOr lib.types.str; default = null; };
        };
      });
      default = [
        { id = "claude"; name = "Claude"; provider = "claude"; }
        { id = "codex"; name = "Codex"; provider = "codex"; }
        { id = "antigravity"; name = "Antigravity"; provider = "antigravity"; }
      ];
    };

    config = {
    # Darwin takes the app from the Homebrew cask (see the brew module): the
    # nix package installs a bare binary, with no Herdr.app bundle, so macOS
    # gives it no Spotlight entry and no proper pasteboard/menu integration.
    # Only the binary differs; the mods below are installed the same way on
    # both platforms, since the installer just copies files.
    home.packages = lib.optionals pkgs.stdenv.hostPlatform.isLinux [
      inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.herdr-app
    ];

    # Copy rather than symlink: Herdr does not hot-reload symlinked mod folders.
    # Keep generated type files and the app's mutable state outside the store.
    home.activation.herdrAppMods = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      run ${pkgs.python3}/bin/python3 ${./install-mods.py} \
        ${lib.escapeShellArg "${config.xdg.configHome}/herdr-app"} \
        ${inputs.herdr-app-jj}/mods \
        ${inputs.herdr-kanso}/open-with/mods/open-with \
        ${inputs.herdr-mods}/filetree/mods \
        ${./mods} \
        ${inputs.herdr-usage-limits}/mods/usage-limits \
        ${accountsJson} \
        ${./main-jj.json}
    '';
    };
  };
}
