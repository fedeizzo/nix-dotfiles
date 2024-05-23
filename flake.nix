{
  description = "My personal NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-rasp.url = "github:nixos/nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    deploy-rs.url = "github:serokell/deploy-rs";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland-contrib = {
      url = "github:hyprwm/contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-lsp-booster = {
      url = "github:slotThe/emacs-lsp-booster-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    impermanence = {
      url = "github:nix-community/impermanence";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    comin = {
      url = "github:nlewo/comin/increase-timeout";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , nixpkgs
    , nixos-hardware
    , flake-utils
    , deploy-rs
    , home-manager
    , impermanence
    , sops-nix
    , ...
    }@inputs:
    let
      lib = import ./lib { inherit inputs; };
      inherit (lib) mkHost forAllSystems;

      macOSPkgs = import inputs.nixpkgs {
        system = "aarch64-darwin";
        overlays = builtins.attrValues {
          emacs = inputs.emacs-overlay.overlays.default;
          emacs-lsp-booster = inputs.emacs-lsp-booster.overlays.default;
          default = import ./overlays { inherit inputs; };
        };
      };
      macOSPkgs-unstable = import inputs.nixpkgs-unstable {
        system = "aarch64-darwin";
        overlays = builtins.attrValues {
          emacs = inputs.emacs-overlay.overlays.default;
          emacs-lsp-booster = inputs.emacs-lsp-booster.overlays.default;
        };
      };
    in
    rec {
      overlays = {
        emacs = inputs.emacs-overlay.overlay;
        emacs-lsp-booster = inputs.emacs-lsp-booster.overlays.default;
        default = import ./overlays { inherit inputs; };
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
        import inputs.nixpkgs-rasp {
          inherit system;
          overlays = builtins.attrValues overlays;
          config.allowUnfree = true;
          config.joypixels.acceptLicense = true;
        }
      );

      # SYSTEM CONFIGS
      nixosConfigurations = {
        fedeizzo-nixos = mkHost {
          username = "fedeizzo";
          hostname = "fedeizzo-nixos";
          system = "x86_64-linux";
          machine = "xps-9510";
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
      homeConfigurations."federico.izzo" = home-manager.lib.homeManagerConfiguration {
        pkgs = macOSPkgs;
        modules = [
          ./home/macbook-pro
        ];
        extraSpecialArgs = {
          pkgs-unstable = macOSPkgs-unstable;
          inputs = inputs;
        };
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
          remoteBuild = true;
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
