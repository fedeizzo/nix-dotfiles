{ inputs, ... }:

let
  system-overlays = {
    emacs-lsp-booster = inputs.emacs-lsp-booster.overlays.default;
    default = import ../overlays {
      inherit inputs;
    };
  };
in
{
  nixosConfigurations = {
    fedeizzo-nixos = inputs.nixpks.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {
        inherit inputs;
        hostname = "fedeizzo-nixos";
        username = "fedeizzo";
        emacs-pkg = import inputs.emacs-pkg { system = "x86_64-linux"; };
      };

      modules = [ ../hosts/xps-9510 ../home/xps-9510 ];
    };
    oven = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";

      specialArgs = {
        inherit inputs;
        hostname = "oven";
        username = "oven";
        emacs-pkg = import inputs.emacs-pkg { system = "x86_64-linux"; };
        system-overlays = system-overlays;
      };

      modules = [ ../hosts/x1-carbon ../home/x1-carbon ];
    };
    rasp-nixos = inputs.nixpkgs-rasp.nixosSystem {
      system = "aarch64-linux";
      specialArgs = {
        inherit inputs;
        hostname = "rasp";
        username = "rasp-nixos";
      };

      modules = [ ../hosts/raspberry ../home/raspberry ];
    };
  };

  darwinConfigurations = {
    COMP-D2G067292T = inputs.nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";

      specialArgs = {
        inherit inputs;
        hostname = "COMP-D2G067292T";
        username = "federico.izzo";
        emacs-pkg = import inputs.emacs-pkg { system = "aarch64-darwin"; };
      };

      modules = [ ../hosts/macbook-pro ];
    };
  };
}
