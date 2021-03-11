{ config, pkgs, ... }:

{
  imports = [ ./common.nix ];

  #################################
  # USER CREATION
  #################################
  users.users.fedeizzo = {
    name = "fedeizzo";
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" "input" "video" "bumblebee" "docker" "autologin" "informant" "users" "networkmanager" "libvirtd" ];
    shell = pkgs.fish;
  };

}
