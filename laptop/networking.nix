{ config, pkgs, ... }:

{
  networking.hostName = "fedeizzo-nixos"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.useDHCP = false;
  networking.interfaces.enp0s3.useDHCP = true;
}
