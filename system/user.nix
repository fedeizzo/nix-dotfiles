{ config, pkgs, ... }:

{
  users.users.fedeizzo = {
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
  };
}
