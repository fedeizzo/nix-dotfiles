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
}
