{
  flake-file.inputs.maki.url = "github:tontinton/maki";

  flake.modules.homeManager.maki = { pkgs, inputs, ... }:
    let
      makiPackage = inputs.maki.packages.${pkgs.stdenv.hostPlatform.system}.default;
    in
    {
      home.packages = [ makiPackage ];

      xdg.configFile = {
        "maki/init.lua".text = ''
          ${builtins.readFile ./config/init.lua}

          ${builtins.readFile ./config/plugins/long-horizon-worker.lua}
        '';
        "maki/AGENTS.md".source = ./config/AGENTS.md;
        "maki/permissions.toml".source = ./config/permissions.toml;
        "maki/plugin.toml".source = ./config/plugin.toml;
        "maki/mcp.toml".source = ./config/mcp.toml;
        "maki/providers.toml".source = ./config/providers.toml;
        "maki/commands".source = ./config/commands;
      };
    };
}
