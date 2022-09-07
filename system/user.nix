{ config, pkgs, ... }:
let
  hashedPassword = "$6$TrvwACA2N3ypzst2$yjczB2TKbWqLWgkQcdoRMjgm09KV5fNVDbm3j.fbFR9g/7l9W.Awpfvqp5P2NFop4L3vj4UCBV1.34cQnOiKt.";
in
{
  users.mutableUsers = if config.fs == "ext4" then true else false;
  programs.fuse.userAllowOther = if config.fs == "btrfs" then true else false;
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
      hashedPassword = if config.fs == "ext4" then null else hashedPassword;
    };
    root = {
      hashedPassword = if config.fs == "ext4" then null else "!";
    };
  };
}
