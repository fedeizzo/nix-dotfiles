{
  description = "My personal NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-rasp.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-duet.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-old.url = "github:nixos/nixpkgs/nixos-21.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    mobile-nixos.flake = false;
    mobile-nixos.url = "github:NixOs/mobile-nixos/master";
    deploy-rs.url = "github:serokell/deploy-rs";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    impermanence = {
      url = "github:nix-community/impermanence";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland = {
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    hyprwm-contrib = {
      url = "github:hyprwm/contrib";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-bubblewrap = {
      url = "sourcehut:~fgaz/nix-bubblewrap";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emanote.url = github:EmaApps/emanote;
    garminDB.url = "github:fedeizzo/GarminDB";
    kindleToOrg.url = github:fedeizzo/KindleToOrg;
  };

  outputs =
    { self
    , nixpkgs
    , nixos-hardware
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
        overlays = builtins.attrValues { emacs = inputs.emacs-overlay.overlays.default; };
      };
      macOSPkgs-unstable = import inputs.nixpkgs-unstable {
        system = "aarch64-darwin";
        overlays = builtins.attrValues { emacs = inputs.emacs-overlay.overlays.default; };
      };
    in
    rec {
      overlays = {
        # emacs = inputs.emacs-overlay.overlay;
        hyperwm-contrib = inputs.hyprwm-contrib.overlays.default;
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
      legacyPackages-duet = forAllSystems (system:
        import inputs.nixpkgs-duet {
          inherit system;
          overlays = builtins.attrValues overlays;
          config.allowUnfree = true;
          config.joypixels.acceptLicense = true;
        }
      );
      nixosConfigurations = {
        fedeizzo-nixos = mkHost {
          username = "fedeizzo";
          syncthing = {
            user = "fedeizzo";
            dataDir = "/persist/home/fedeizzo";
            folders = [
              { name = "University"; role = "sendreceive"; }
              { name = "org"; role = "sendreceive"; }
              { name = "nix-dotfiles"; role = "sendonly"; }
              { name = "videoFiles"; role = "sendonly"; }
            ];
            devices = [
              {
                name = "homelab";
                addresses = [ "tcp://home-lab:22000" ];
                id = "ZJCMEXW-XYDWZ2C-EXAFIXY-KW7RBHR-2Z43SZT-42ZKRY5-TJ3DS46-2JQ3ZAE";
              }
              {
                name = "smartphone";
                addresses = [ "tcp://phone:22000" ];
                id = "3IP3HFM-N3EOJRE-5TLKYS2-ZJ6C6OA-TBB7SDT-QBZ6XZF-J45SF4O-DDG72AH";
              }
            ];
          };
          hostname = "fedeizzo-nixos";
          fs = "btrfs";
          system = "x86_64-linux";
          machine = "xps-9510";
          pkgs = legacyPackages."x86_64-linux";
        };
        rasp-nixos = mkHost {
          username = "rasp";
          syncthing = {
            user = "sync";
            role = "sendreceive";
            dataDir = "/home/sync";
            folders = [
              { name = "University"; role = "sendreceive"; }
              { name = "org"; role = "sendreceive"; }
              { name = "nix-dotfiles"; role = "receiveonly"; }
              { name = "videoFiles"; role = "receiveonly"; }
            ];
            devices = [
              {
                name = "laptop";
                addresses = [ "tcp://laptop:22000" ];
                id = "PUBABVB-EZQRX62-AUPAK5C-FFB5UUW-KVJDFVI-SUZ43J4-USRJTNE-WSHGQA7";
                introducer = true;
              }
              {
                name = "smartphone";
                addresses = [ "tcp://phone:22000" ];
                id = "3IP3HFM-N3EOJRE-5TLKYS2-ZJ6C6OA-TBB7SDT-QBZ6XZF-J45SF4O-DDG72AH";
              }
            ];
          };
          hostname = "rasp-nixos";
          fs = "ext4";
          system = "aarch64-linux";
          machine = "raspberry";
          pkgs = legacyPackages-rasp."aarch64-linux";
        };
        duet-nixos = mkHost {
          username = "nixtab";
          syncthing = {
            user = "nixtab";
            role = "sendreceive";
            dataDir = "/home/nixtab";
            folders = [
              { name = "University"; role = "sendreceive"; }
              { name = "org"; role = "sendreceive"; }
              { name = "nix-dotfiles"; role = "receiveonly"; }
              { name = "videoFiles"; role = "receiveonly"; }
            ];
            devices = [
              {
                name = "homelab";
                addresses = [ "tcp://home-lab:22000" ];
                id = "ZJCMEXW-XYDWZ2C-EXAFIXY-KW7RBHR-2Z43SZT-42ZKRY5-TJ3DS46-2JQ3ZAE";
                introducer = true;
              }
              {
                name = "laptop";
                addresses = [ "tcp://laptop:22000" ];
                id = "PUBABVB-EZQRX62-AUPAK5C-FFB5UUW-KVJDFVI-SUZ43J4-USRJTNE-WSHGQA7";
                introducer = true;
              }
            ];
          };
          hostname = "duet-nixos";
          fs = "ext4";
          system = "aarch64-linux";
          machine = "duet";
          pkgs = legacyPackages-duet."aarch64-linux";
        };
      };
      homeConfigurations."federico.izzo" = home-manager.lib.homeManagerConfiguration {
        pkgs = macOSPkgs;
        modules = [
          ./home/macbook-pro
        ];
        extraSpecialArgs = {
          pkgs-unstable = macOSPkgs-unstable;
        };
      };
      deploy.nodes = {
        rasp-nixos = {
          hostname = "home-lab";
          sshUser = "root";
          sudo = "doas -u";
          sshOpts = [ ];
          magicRollback = true;
          autoRollback = true;
          fastConnection = false;
          profiles.system = {
            user = "root";
            path = deploy-rs.lib.aarch64-linux.activate.nixos self.nixosConfigurations.rasp-nixos;
          };
        };
        duet-nixos = {
          hostname = "duet";
          sshUser = "root";
          sudo = "doas -u";
          sshOpts = [ ];
          magicRollback = true;
          autoRollback = false;
          fastConnection = false;
          profiles.system = {
            user = "root";
            path = deploy-rs.lib.aarch64-linux.activate.nixos self.nixosConfigurations.duet-nixos;
          };
        };
      };
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
      templates = {
        python = {
          path = ./templates/python-mach-nix;
          description = "A white python mach-nix project";
        };
      };
    };
}


