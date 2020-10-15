{ config, pkgs, ... }:

{
  networking.hostName = "fedeizzo-nixos"; # Define your hostname.
  # networking.wireless.iwd.enable = true;
  networking.networkmanager.enable = true;
  networking.useDHCP = false;
}
