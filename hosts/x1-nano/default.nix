{ modulesPath, inputs, username, ... }:

{
  imports = [
    inputs.impermanence.nixosModules.impermanence
    inputs.home-manager.nixosModules.home-manager
    inputs.sops-nix.nixosModules.sops
    inputs.nix-topology.nixosModules.default
    (modulesPath + "/installer/scan/not-detected.nix")

    inputs.self.modules.nixos.bluetooth
    inputs.self.modules.nixos.boot
    ./system/disk.nix
    inputs.self.modules.nixos.disko-x1-nano
    inputs.self.modules.nixos.environment
    inputs.self.modules.nixos.hardware
    inputs.self.modules.nixos.misc
    ./system/networking.nix
    ./system/persistent.nix
    ./system/powerThermal.nix
    ./system/security.nix
    ./system/user.nix
    ./system/videoAudio.nix
    inputs.self.modules.nixos.fish
    inputs.self.modules.nixos.nh
    inputs.self.modules.nixos.nix
    inputs.self.modules.nixos.solaar
    inputs.self.modules.nixos.sops
    inputs.self.modules.nixos.tailscale
    inputs.self.modules.nixos.wireguard
  ];

  system.stateVersion = "25.05";
}
