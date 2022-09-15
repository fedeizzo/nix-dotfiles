{ config, pkgs, ... }:

{
  networking.hostName = config.hostname;
  networking.networkmanager.enable = true;
  networking.useDHCP = false;
  networking.firewall = {
    enable = true;
    checkReversePath = "loose";
  };
  services.tailscale.enable = true;
}
