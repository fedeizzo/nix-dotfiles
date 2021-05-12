{ config, pkgs, ... }:

{
  #################################
  # NETWORKING
  #################################
  networking.hostName = "fedeizzo-nixos";
  networking.networkmanager.enable = true;
  networking.useDHCP = false;

}
