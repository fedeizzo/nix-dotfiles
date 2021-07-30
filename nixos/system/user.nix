{ config, pkgs, ... }:

{
  #################################
  # USER CREATION
  #################################
  users.users.fedeizzo = {
    name = "fedeizzo";
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" "input" "video" "bumblebee" "docker" "autologin" "users" "networkmanager" "libvirtd" "steamps4" ];
    shell = pkgs.fish;
  };
}
