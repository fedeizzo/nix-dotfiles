{ username, config, fs, pkgs, sops, ... }:

{
  users.mutableUsers = if fs == "ext4" then true else false;
  programs.fuse.userAllowOther = if fs == "btrfs" then true else false;
  users.users = {
    ${username} = {
      name = username;
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
        "keys" # required to have read access to /run/secrets.d (sops-nix)
      ];
      shell = pkgs.fish;
      passwordFile = if fs == "ext4" then null else config.sops.secrets.fedeizzo-path.path;
    };
    root = {
      hashedPassword = "!"; # to enable root login remode this line
      # passwordFile = if config.fs == "ext4" then null else config.sops.secrets.root-path.path;
    };
  };
}
