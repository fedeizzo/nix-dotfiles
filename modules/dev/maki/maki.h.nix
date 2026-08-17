{
  flake-file.inputs.maki.url = "github:tontinton/maki";

  flake.modules.homeManager.maki = { pkgs, inputs, ... }: {
    home.packages = [
      inputs.maki.packages.${pkgs.stdenv.hostPlatform.system}.default
    ];

    xdg.configFile = {
      "maki/init.lua".source = ./config/init.lua;
      "maki/AGENTS.md".source = ./config/AGENTS.md;
      "maki/permissions.toml".source = ./config/permissions.toml;
      "maki/mcp.toml".source = ./config/mcp.toml;
      "maki/providers.toml".source = ./config/providers.toml;
      "maki/commands".source = ./config/commands;
      "maki/plugins".source = ./config/plugins;
    };
  };
}
