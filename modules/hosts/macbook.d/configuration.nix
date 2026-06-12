{ inputs, self, ... }:

{
  flake.darwinConfigurations.COMP-LNY95W42WQ = inputs.nix-darwin.lib.darwinSystem {
    system = "aarch64-darwin";

    specialArgs = rec {
      inherit inputs;
      hostname = "COMP-LNY95W42WQ";
      username = "federico.izzo";
      pkgs-unstable = import inputs.nixpkgs-unstable {
        system = "aarch64-darwin";
        config.allowUnfree = true;
      };
    };

    modules = [
      self.modules.darwin.macbook
    ];
  };
}
