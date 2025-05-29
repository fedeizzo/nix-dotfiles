{ inputs, ... }:

let
  system-overlays = {
    emacs-lsp-booster = inputs.emacs-lsp-booster.overlays.default;
    default = import ../overlays {
      inherit inputs;
    };
    nix-topology = inputs.nix-topology.overlays.default;
  };
in
{
  nixosConfigurations = {
    homelab = inputs.nixpkgs-homelab.lib.nixosSystem rec {
      system = "x86_64-linux";
      specialArgs = {
        inherit inputs;
        hostname = "homelab";
        username = "homelab";
        pkgs-unstable = import inputs.nixpkgs-homelab-unstable { inherit system; };
      };

      modules = [
        # TODO: remove after jellyseerr is updated to v2 in 22.11
        {
          imports = [
            (inputs.nixpkgs-homelab-unstable + /nixos/modules/services/misc/jellyseerr.nix)
            (inputs.nixpkgs-homelab-unstable + /nixos/modules/services/home-automation/home-assistant.nix)
            (inputs.nixpkgs-homelab-unstable + /nixos/modules/services/misc/paperless.nix)
          ];
          nixpkgs.overlays = [
            (_: _: {
              inherit (inputs.nixpkgs-homelab-unstable.legacyPackages.${system}) jellyseerr;
              inherit (inputs.nixpkgs-homelab-unstable.legacyPackages.${system}) home-assistant;
              inherit (inputs.nixpkgs-homelab-unstable.legacyPackages.${system}) paperless;
            })
          ];
          disabledModules = [
            "services/misc/jellyseerr.nix"
            "services/home-automation/home-assistant.nix"
            "services/misc/paperless.nix"
          ];
        }
        ../hosts/xps-9510-homelab
      ];
    };
    oven = inputs.nixpkgs.lib.nixosSystem rec{
      system = "x86_64-linux";

      specialArgs = rec {
        inherit inputs;
        hostname = "oven";
        username = "oven";
        emacs-pkg = import inputs.emacs-pkg { inherit system; };
        pkgs-unstable = import inputs.nixpkgs-unstable {
          inherit system;
          config.allowUnfree = true;
        };
        anytype-pkgs = import inputs.anytype-pkgs {
          inherit system;
          config.allowUnfree = true;
        };

        inherit system-overlays;
      };

      modules = [ ../hosts/x1-nano ../home/x1-nano ];
    };
    # rasp = inputs.nixpkgs-homelab.lib.nixosSystem {
    #   system = "aarch64-linux";
    #   specialArgs = {
    #     inherit inputs;
    #     hostname = "rasp";
    #     username = "rasp";
    #   };

    #   modules = [ ../hosts/raspberry ../home/raspberry ];
    # };
  };

  darwinConfigurations = {
    COMP-D2G067292T = inputs.nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";

      specialArgs = rec {
        inherit inputs;
        hostname = "COMP-D2G067292T";
        username = "federico.izzo";
        emacs-pkg = import inputs.emacs-pkg { system = "aarch64-darwin"; };
        pkgs-unstable = import inputs.nixpkgs-unstable {
          system = "aarch64-darwin";
          config.allowUnfree = true;
        };
        inherit system-overlays;
      };

      modules = [ ../hosts/macbook-pro ];
    };
  };
}
