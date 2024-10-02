{
  description = "My personal NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-24.05-darwin";
    nixpkgs-rasp.url = "github:nixos/nixpkgs/nixos-24.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs-darwin";
    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nh-darwin.url = "github:ToyVo/nh-darwin";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";
    deploy-rs.url = "github:serokell/deploy-rs";
    impermanence.url = "github:nix-community/impermanence";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    comin.url = "github:nlewo/comin/increase-timeout";
    comin.inputs.nixpkgs.follows = "nixpkgs";


    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-lsp-booster = {
      url = "github:slotThe/emacs-lsp-booster-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Wayland and Hyprland
    vigiland.url = "github:jappie3/vigiland";
    hyprland-contrib.url = "github:hyprwm/contrib";
    hyprland-contrib.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-darwin
    , nixpkgs-rasp
    , nixos-hardware
    , home-manager
    , deploy-rs
    , flake-utils
    , impermanence
    , sops-nix
    , ...
    }@inputs:
    let
      lib = import ./lib { inherit inputs; };
      inherit (lib) mkHost forAllSystems;

    in
    rec {
      overlays = {
        emacs = inputs.emacs-overlay.overlay;
        emacs-lsp-booster = inputs.emacs-lsp-booster.overlays.default;
        default = import ./overlays {
          inherit inputs;
        };
      };
      legacyPackages = forAllSystems (system:
        import inputs.nixpkgs {
          inherit system;
          overlays = builtins.attrValues overlays;
          config.allowUnfree = true;
          config.joypixels.acceptLicense = true;
        }
      );
      legacyPackages-rasp = forAllSystems (system:
        import nixpkgs-rasp {
          inherit system;
          overlays = builtins.attrValues overlays;
          config.allowUnfree = true;
          config.joypixels.acceptLicense = true;
        }
      );
      legacyPackages-macos = import inputs.nixpkgs-darwin {
        system = "aarch64-darwin";
        overlays = builtins.attrValues overlays;
        config.allowUnfree = true;
      };

      # SYSTEM CONFIGS
      nixosConfigurations = {
        fedeizzo-nixos = mkHost {
          username = "fedeizzo";
          hostname = "fedeizzo-nixos";
          system = "x86_64-linux";
          machine = "xps-9510";
          pkgs = legacyPackages."x86_64-linux";
        };
        oven = mkHost {
          username = "oven";
          hostname = "oven-nixos";
          system = "x86_64-linux";
          machine = "x1-carbon";
          pkgs = legacyPackages."x86_64-linux";
        };
        rasp-nixos = mkHost {
          username = "rasp";
          hostname = "rasp-nixos";
          system = "aarch64-linux";
          machine = "raspberry";
          pkgs = legacyPackages-rasp."aarch64-linux";
        };
      };
      darwinConfigurations."COMP-D2G067292T" = mkHost {
        username = "federico.izzo";
        hostname = "COMP-D2G067292T";
        system = "aarch64-darwin";
        machine = "macbook-pro";
        pkgs = legacyPackages-macos;
        isMac = true;
      };

      # REMOTE DEPLOY
      deploy.nodes = {
        rasp-nixos = {
          hostname = "homelab";
          sshUser = "root";
          sudo = "doas -u";
          sshOpts = [ ];
          magicRollback = true;
          autoRollback = true;
          fastConnection = false;
          remoteBuild = false;
          profiles.system = {
            user = "root";
            path = deploy-rs.lib.aarch64-linux.activate.nixos self.nixosConfigurations.rasp-nixos;
          };
        };
      };

      # OTHER FLAKEs FIELDS
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

      templates = {
        python = {
          path = ./templates/python-mach-nix;
          description = "A white python mach-nix project";
        };
      };
    } // flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShells.default = pkgs.mkShell {
        packages = [
          pkgs.deploy-rs
          pkgs.sops
        ];
        shellHook = ''
          export PATH=$PATH:$(pwd)/scripts
        '';
      };
    }
    );
}
