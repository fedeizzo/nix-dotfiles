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
    flake-utils.url = "github:numtide/flake-utils";

    # Installation and boot
    impermanence.url = "github:nix-community/impermanence";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    # Remote deployment and secretes
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    comin.url = "github:nlewo/comin/increase-timeout";
    comin.inputs.nixpkgs.follows = "nixpkgs";
    deploy-rs.url = "github:serokell/deploy-rs";

    # Misc
    emacs-pkg.url = "github:nixos/nixpkgs/b805fe3d6f3e702ecee01710ee552e3ed39d16c8";
    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
    emacs-lsp-booster.inputs.nixpkgs.follows = "nixpkgs";
    nh-darwin.url = "github:ToyVo/nh-darwin";

    # Wayland and Hyprland
    vigiland.url = "github:jappie3/vigiland";
    hyprland-contrib.url = "github:hyprwm/contrib";
    hyprland-contrib.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
  };

  outputs =
    { self
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
        emacs-lsp-booster = inputs.emacs-lsp-booster.overlays.default;
        default = import ./overlays {
          inherit inputs;
        };
      };
      pkgs = forAllSystems (system:
        import inputs.nixpkgs {
          inherit system;
          overlays = builtins.attrValues overlays;
          config.allowUnfree = true;
          config.joypixels.acceptLicense = true;
        }
      );
      emacs-pkg = forAllSystems (system:
        import inputs.emacs-pkg {
          inherit system;
          overlays = builtins.attrValues overlays;
          config.allowUnfree = true;
          config.joypixels.acceptLicense = true;
        }
      );
      pkgs-rasp = forAllSystems (system:
        import inputs.nixpkgs-rasp {
          inherit system;
          overlays = builtins.attrValues overlays;
          config.allowUnfree = true;
          config.joypixels.acceptLicense = true;
        }
      );
      pkgs-macos = import inputs.nixpkgs-darwin {
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
          pkgs = pkgs."x86_64-linux";
	  emacs-pkg = emacs-pkg."x86_64-linux";
        };
        oven = mkHost {
          username = "oven";
          hostname = "oven";
          system = "x86_64-linux";
          machine = "x1-carbon";
          pkgs = pkgs."x86_64-linux";
	  emacs-pkg = emacs-pkg."x86_64-linux";
        };
        rasp-nixos = mkHost {
          username = "rasp";
          hostname = "rasp-nixos";
          system = "aarch64-linux";
          machine = "raspberry";
          pkgs = pkgs-rasp."aarch64-linux";
	  emacs-pkg = emacs-pkg."aarch64-linux";
        };
      };
      darwinConfigurations."COMP-D2G067292T" = mkHost {
        username = "federico.izzo";
        hostname = "COMP-D2G067292T";
        system = "aarch64-darwin";
        machine = "macbook-pro";
        pkgs = pkgs-macos;
	emacs-pkg = emacs-pkg."aarch64-darwing";
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
      pkgs = inputs.nixpkgs.legacyPackages.${system};
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
