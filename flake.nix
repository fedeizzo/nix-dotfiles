{
  description = "My personal NixOS configuration";

  inputs = {
    # Nixpkgs
    ##X1
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/release-25.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    solaar = {
      url = "github:Svenum/Solaar-Flake/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ## Mac
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-26.05-darwin";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nix-darwin.url = "github:LnL7/nix-darwin/nix-darwin-26.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs-darwin";

    ## Homelab
    nixpkgs-homelab.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-homelab-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    ## Raspberry
    nixos-pikvm.url = "github:hatch01/nixos-pikvm";

    # Flake management
    flake-parts.url = "github:hercules-ci/flake-parts";
    devshell.url = "github:numtide/devshell";
    git-hooks-nix.url = "github:cachix/git-hooks.nix";
    nix-topology.url = "github:oddlama/nix-topology";
    nixos-generators.url = "github:nix-community/nixos-generators";
    import-tree.url = "github:vic/import-tree";

    # Installation and boot
    impermanence.url = "github:nix-community/impermanence";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    # Remote deployment and secretes
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    deploy-rs.url = "github:serokell/deploy-rs";

    # Misc
    nh.url = "github:nix-community/nh";
    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    nur.inputs.nixpkgs.follows = "nixpkgs";
    zen-browser.url = "github:0xc000022070/zen-browser-flake";
    authentik-nix.url = "github:nix-community/authentik-nix";
    nix-amd-ai.url = "github:noamsto/nix-amd-ai";
    hermes-agent.url = "github:NousResearch/hermes-agent";
    ds4.url = "github:fedeizzo/ds4";

    # Wayland and niri
    stylix.url = "github:danth/stylix";
    niri.url = "github:sodiboo/niri-flake";
    dms.url = "github:AvengeMedia/DankMaterialShell/stable";
    dms-plugin-registry.url = "github:AvengeMedia/dms-plugin-registry";

    # Sandboxing
    llm-agents.url = "github:numtide/llm-agents.nix";
    jail-nix.url = "sourcehut:~alexdavid/jail.nix";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake
      { inherit inputs; }
      {
        imports = [
          inputs.flake-parts.flakeModules.modules
          inputs.devshell.flakeModule
          inputs.git-hooks-nix.flakeModule
          inputs.nix-topology.flakeModule
          (inputs.import-tree ./modules)
        ];
        systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
        perSystem = { pkgs, ... }: {
          formatter = pkgs.nixpkgs-fmt;
          imports = [
            ./nix/git-hooks.nix
            ./nix/devshells.nix
            ./nix/topology.nix
          ];
        };
        flake = {
          imports = [
            (import ./nix/packages.nix { inherit inputs self; system = "x86_64-linux"; })
          ];
          checks = builtins.mapAttrs (_system: deployLib: deployLib.deployChecks self.deploy) inputs.deploy-rs.lib;
        };
      };
}
