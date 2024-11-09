{ username, pkgs, config, ... }:

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
      hashedPasswordFile = config.sops.secrets."${username}-path".path;
    };
    root = {
      hashedPassword = "!";
    };
  };

  services = {
    xserver.enable = true;
    displayManager.sddm.enable = true;
    desktopManager.plasma6.enable = true;
    displayManager.sddm.wayland.enable = true;
  };
  environment.plasma6.excludePackages = with pkgs.kdePackages; [
    plasma-browser-integration
    konsole
    ark
    elisa
    gwenview
    okular
    kate
    khelpcenter
    print-manager
  ];
}
