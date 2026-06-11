{ inputs, self, ... }:

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
  flake.darwinConfigurations.COMP-LNY95W42WQ = inputs.nix-darwin.lib.darwinSystem {
    system = "aarch64-darwin";

    specialArgs = rec {
      inherit inputs;
      hostname = "COMP-LNY95W42WQ";
      username = "federico.izzo";
      emacs-pkg = import inputs.emacs-pkg { system = "aarch64-darwin"; };
      pkgs-unstable = import inputs.nixpkgs-unstable {
        system = "aarch64-darwin";
        config.allowUnfree = true;
      };
      inherit system-overlays;
    };

    modules = [
      self.modules.darwin.macbook
    ];
  };
}
