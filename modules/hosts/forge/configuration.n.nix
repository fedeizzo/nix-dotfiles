{ inputs, self, ... }:

let
  system-overlays = {
    default = import ../../../overlays {
      inherit inputs;
    };
    nix-topology = inputs.nix-topology.overlays.default;
  };
in
{
  flake.nixosConfigurations.forge = inputs.nixpkgs.lib.nixosSystem rec {
    system = "x86_64-linux";

    specialArgs = {
      inherit inputs self;
      hostname = "forge";
      username = "mixer";
      pkgs-unstable = import inputs.nixpkgs-unstable { inherit system; };
      inherit system-overlays;
    };

    modules = [
      self.modules.nixos.forge
    ];
  };

  # Second Framework Desktop: compute node, only llama-swap for now.
  # Ideas for growing it are in ./forge-ideas.md.
  flake.modules.nixos.forge = {
    imports = [
      inputs.self.modules.nixos.strix-halo
      inputs.self.modules.nixos.server
      inputs.self.modules.nixos.ephemeral-root

      inputs.self.modules.nixos.misc
      inputs.self.modules.nixos.nix
      inputs.self.modules.nixos.security
      inputs.self.modules.nixos.disk
      inputs.self.modules.nixos.power
      inputs.self.modules.nixos.hardware
      inputs.self.modules.nixos.boot
      inputs.self.modules.nixos.zsh

      inputs.self.modules.nixos.networking
      inputs.self.modules.nixos.sops
      inputs.self.modules.nixos.environment
      inputs.self.modules.nixos.persistent

      inputs.self.modules.nixos.starship

      inputs.self.modules.nixos.fi-services
      inputs.self.modules.nixos.llama-swap
    ];

    system.stateVersion = "26.05";
  };
}
