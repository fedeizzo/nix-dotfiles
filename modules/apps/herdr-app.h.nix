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

  flake.modules.homeManager.herdr-app = { pkgs, lib, inputs, config, ... }: {
    imports = [ inputs.self.modules.homeManager.collie ];
    xdg.configFile."herdr-app/mods/collie".source = config.fi.collie.modSource;

    home.packages = [
      inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.herdr-app
    ];

    # Copy rather than symlink: Herdr does not hot-reload symlinked mod folders.
    # Keep generated type files and the app's mutable state outside the store.
    home.activation.herdrAppMods = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      run ${pkgs.python3}/bin/python3 ${./herdr-app/install-mods.py} \
        ${lib.escapeShellArg "${config.xdg.configHome}/herdr-app"} \
        ${inputs.herdr-app-jj}/mods \
        ${inputs.herdr-kanso}/open-with/mods/open-with \
        ${inputs.herdr-mods}/filetree/mods \
        ${./herdr-app}
    '';
  };
}
