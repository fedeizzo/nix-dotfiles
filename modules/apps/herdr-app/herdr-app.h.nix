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
      cfg = config.fi.herdrApp;
      json = lib.types.attrsOf lib.types.anything;
      hasExtras = cfg.extraMods != { } || cfg.extraLayouts != { }
        || cfg.extraFloating != [ ] || cfg.extraKeys != { };
      # Interpolating the paths copies them into the store; the installer then
      # reads absolute paths only.
      extras = pkgs.writeText "herdr-app-extras.json" (builtins.toJSON {
        mods = lib.mapAttrs (_: path: "${path}") cfg.extraMods;
        layouts = lib.mapAttrs (_: path: "${path}") cfg.extraLayouts;
        floating = cfg.extraFloating;
        keys = cfg.extraKeys;
      });
    in
    {
      # What a profile adds on top of the shared setup (see profiles/work). All
      # empty by default: hosts that set nothing run the installer exactly as before.
      options.fi.herdrApp = {
        extraMods = lib.mkOption {
          type = lib.types.attrsOf lib.types.path;
          default = { };
          description = "More mod folders to install, by mod name.";
        };
        extraLayouts = lib.mkOption {
          type = lib.types.attrsOf lib.types.path;
          default = { };
          description = "More layout files to install, by layout name.";
        };
        extraFloating = lib.mkOption {
          type = lib.types.listOf json;
          default = [ ];
          description = ''
            Floating blocks added to the root of main-jj and of every extra layout
            (by id), e.g. an invisible mod that must run whatever is shown.
          '';
        };
        extraKeys = lib.mkOption {
          type = lib.types.attrsOf json;
          default = { };
          description = ''
            keys.json bindings by section. Merged like the built-in ones: a key the
            user already bound keeps its binding.
          '';
        };
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
            ${./main-jj.json}${lib.optionalString hasExtras " \\\n  ${extras}"}
        '';
      };
    };
}
