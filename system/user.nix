{ config, pkgs, sops, ... }:

{
  users.mutableUsers = if config.fs == "ext4" then true else false;
  programs.fuse.userAllowOther = if config.fs == "btrfs" then true else false;
  sops.secrets.fedeizzo-path = {
    neededForUsers = true;
  };
  sops.secrets.root-path = {
    neededForUsers = true;
  };
  users.users = {
    ${config.username} = {
      name = config.username;
      isNormalUser = true;
      createHome = true;
      extraGroups = [
        "wheel"
        "input"
        "video"
        "bumblebee"
        "docker"
        "users"
        "networkmanager"
        "libvirtd"
        "audio"
        "dialout" # used to allow flash over serial port without root user
        "adbusers" # for adb android
      ];
      shell = pkgs.fish;
      passwordFile = if config.fs == "ext4" then null else config.sops.secrets.fedeizzo-path.path;
    };
    root = {
      passwordFile = if config.fs == "ext4" then null else config.sops.secrets.root-path.path;
    };
  };
}
