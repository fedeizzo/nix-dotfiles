{ config, pkgs, ... }:

{
  users.users.fedeizzo = {
    name = "fedeizzo";
    isNormalUser = true;
    createHome = true;
    extraGroups = [
      "wheel"
      "input"
      "video"
      "bumblebee"
      "docker"
      "autologin"
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
