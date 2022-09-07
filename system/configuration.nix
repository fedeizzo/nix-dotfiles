{ config, pkgs, ... }:

{
  imports = [
    ./boot.nix
    ./hardware.nix
    # (./hardware-configurations + "/${config.fs}.nix")
    ./hardware-configurations/ext4.nix
    ./keymapTimeFont.nix
    ./pipewire.nix
    ./networking.nix
    ./nixNixOS.nix
    ./nvidia.nix
    ./programsEnv.nix
    ./security.nix
    ./services.nix
    ./user.nix
    if config.fs == "btrfs" then ./persistent.nix else null
  ];
}
