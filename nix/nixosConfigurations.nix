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
          ];
          nixpkgs.overlays = [
            (_: _: {
              inherit (inputs.nixpkgs-homelab-unstable.legacyPackages.${system}) jellyseerr;
            })
          ];
          disabledModules = [
            "services/misc/jellyseerr.nix"
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
        pkgs-old = import inputs.nixpkgs-old { inherit system; };
        pkgs-unstable = import inputs.nixpkgs-unstable {
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

      specialArgs = {
        inherit inputs;
        hostname = "COMP-D2G067292T";
        username = "federico.izzo";
        emacs-pkg = import inputs.emacs-pkg { system = "aarch64-darwin"; };
        inherit system-overlays;
      };

      modules = [ ../hosts/macbook-pro ];
    };
  };
}
