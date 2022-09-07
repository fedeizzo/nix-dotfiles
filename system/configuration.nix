{ config, modulesPath, ... }:

{
  imports = [
    ./boot.nix
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

    ./hardware-configurations/ext4.nix
    # erase on boot imported only with fs = "btrfs"
    ./hardware-configurations/btrfs.nix
    ./persistent.nix
    (modulesPath + "/installer/scan/not-detected.nix")
  ];
}
