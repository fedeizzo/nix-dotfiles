{
  flake-file.inputs.solaar.url = "github:Svenum/Solaar-Flake/main";
  flake-file.inputs.solaar.inputs.nixpkgs.follows = "nixpkgs";

  flake.modules.nixos.solaar = { pkgs, inputs, ... }: {
    imports = [ inputs.solaar.nixosModules.default ];

    services.solaar = {
      enable = true;
      package = pkgs.solaar;
      window = "hide";
      batteryIcons = "solaar";
      extraArgs = "";
    };
  };

  flake.modules.homeManager.solaar = { ... }: {
    xdg.configFile."solaar/config.yaml".source = ./config.yaml;
    xdg.configFile."solaar/rules.yaml".source = ./rules.yaml;
  };
}
