{ inputs, self, ... }:

let
  system-overlays = {
    emacs-lsp-booster = inputs.emacs-lsp-booster.overlays.default;
    default = import ../../../overlays {
      inherit inputs;
    };
    nix-topology = inputs.nix-topology.overlays.default;
  };
in
{
  flake.nixosConfigurations.oven = inputs.nixpkgs.lib.nixosSystem rec {
    system = "x86_64-linux";

    specialArgs = rec {
      inherit inputs self;
      hostname = "oven";
      username = "oven";
      pkgs-unstable = import inputs.nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
      };

      inherit system-overlays;
    };

    modules = [
      self.modules.nixos.x1-nano
    ];
  };

  flake.modules.nixos.x1-nano = { modulesPath, ... }: {
    imports = [
      (modulesPath + "/installer/scan/not-detected.nix")

      inputs.self.modules.nixos.bluetooth
      inputs.self.modules.nixos.boot
      inputs.self.modules.nixos.disk
      inputs.self.modules.nixos.disko-x1-nano
      inputs.self.modules.nixos.desktop-environment
      inputs.self.modules.nixos.environment
      inputs.self.modules.nixos.hardware
      inputs.self.modules.nixos.misc
      inputs.self.modules.nixos.networking
      inputs.self.modules.nixos.persistent
      inputs.self.modules.nixos.power
      inputs.self.modules.nixos.security
      inputs.self.modules.nixos.user
      inputs.self.modules.nixos.media
      inputs.self.modules.nixos.fish
      inputs.self.modules.nixos.nh
      inputs.self.modules.nixos.nix
      inputs.self.modules.nixos.solaar
      inputs.self.modules.nixos.sops
      inputs.self.modules.nixos.tailscale
      inputs.self.modules.nixos.wireguard
    ];

    system.stateVersion = "25.05";
  };
}
