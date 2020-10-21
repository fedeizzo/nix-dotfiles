{ config, pkgs, ... }:

let
  impermanence = builtins.fetchTarball {
    url =
      "https://github.com/nix-community/impermanence/archive/master.tar.gz";
  };
in
{
  imports = [ ./common.nix "${impermanence}/nixos.nix" ];
  #################################
  # USER CREATION
  #################################
  users.mutableUsers = false;
  users.users.root.initialPassword = "pass";
  users.users.fedeizzo = {
    name = "fedeizzo";
    initialPassword = "pass";
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" "input" "video" "bumblebee" "docker" "autologin" "informant" "users" "networkmanager" ];
    shell = pkgs.zsh;
  };

  #################################
  # PERSISTENT FILES
  #################################
  environment.persistence."/nix/persistence" = {
    directories = [
      "/etc/NetworkManager/system-connections"
      "/etc/nixos"
      "/var/log"
      "/var/cache"
      "/var/tmp"
      "/var/lib/machines"
      "/var/lib/portables"
      "/var/lib/misc"
      "/var/lib/postgresql"
      "/var/lib/systemd"
      "/var/lib/docker"
      "/var/lib/bluetooth"
      "/swap"
    ];
    files = [
      "/etc/machine-id"
    ];
  };
}
