{ config, pkgs, ... }:

let
  home-manager = builtins.fetchTarball {
    url = "https://github.com/rycee/home-manager/archive/release-20.09.tar.gz";
  };
  impermanence = builtins.fetchTarball {
    url =
      "https://github.com/nix-community/impermanence/archive/master.tar.gz";
  };
in
{
  imports = [ ./common.nix "${impermanence}/nixos.nix" "${home-manager}/nixos" ];
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
    shell = pkgs.fish;
  };

  #################################
  # PERSISTENT FILES
  #################################
  environment.persistence."/nix/persistent" = {
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

  home-manager.users.fedeizzo = { pkgs, ... }: {
    imports = [ "${impermanence}/home-manager.nix" ];

    programs.home-manager.enable = true;

    home.persistence."/nix/persistent/home/fedeizzo" = {
    directories = [ ".cache" ".local/share" ".mozilla" ".ssh" "persitent" ];
    # files = [ ".zsh_history" ];
    };
  };
}
