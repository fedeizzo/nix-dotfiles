{ username, pkgs, ... }:

{
  users.mutableUsers = false;
  programs.fuse.userAllowOther = true;
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
    };
    root = {
      hashedPassword = "!";
    };
  };
}
