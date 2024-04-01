{ config, modulesPath, inputs, ... }:

{
  imports = [
    inputs.impermanence.nixosModules.impermanence
    inputs.home-manager.nixosModules.home-manager
    inputs.sops-nix.nixosModules.sops
    ./boot.nix
    ./borg.nix
    ./hardware.nix
    ./keymapTimeFont.nix
    ./pipewire.nix
    ./networking.nix
    ./nixNixOS.nix
    ./nvidia.nix
    ./programsEnv.nix
    ./security.nix
    ./services.nix
    ./user.nix
    # ./gnome.nix

    ./hardware-configurations/ext4.nix
    # erase on boot imported only with fs = "btrfs"
    ./hardware-configurations/btrfs.nix
    ./persistent.nix
    (modulesPath + "/installer/scan/not-detected.nix")
  ];
}
