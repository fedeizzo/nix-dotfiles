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
        (import ../hosts/framework-desktop/system/module-override.nix {
          pkgs-unstable = inputs.nixpkgs-unstable;
          inherit system;
        })
        ../hosts/framework-desktop
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

        inherit system-overlays;
      };

      modules = [ ../hosts/x1-nano ../home/x1-nano ];
    };
    freezer = inputs.nixpkgs-homelab.lib.nixosSystem {
      system = "aarch64-linux";
      specialArgs = {
        inherit inputs;
        hostname = "freezer";
        username = "freezer";
      };

      modules = [ ../hosts/raspberry ../home/raspberry ];
    };
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
