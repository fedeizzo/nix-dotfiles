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
    homelab = inputs.nixpkgs-homelab.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {
        inherit inputs;
        hostname = "homelab";
        username = "homelab";
      };

      modules = [ ../hosts/xps-9510-homelab ];
    };
    oven = inputs.nixpkgs.lib.nixosSystem rec{
      system = "x86_64-linux";

      specialArgs = rec {
        inherit inputs;
        hostname = "oven";
        username = "oven";
        emacs-pkg = import inputs.emacs-pkg { inherit system; };
        pkgs-old = import inputs.nixpkgs-old { inherit system; };
        inherit system-overlays;
      };

      modules = [ ../hosts/x1-carbon ../home/x1-carbon ];
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
