{ config, pkgs, ... }:

{
  networking.hostName = config.hostname;
  networking.networkmanager.enable = true;
  networking.useDHCP = false;
  services.tailscale.enable = true;
}
