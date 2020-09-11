{ config, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    autorun = true;
    desktopManager.default = "none";
    # displayManager.defaultSession = "lightdm";
    # displayManager.job.execCmd = "${pkgs.lighdm}/bin/lightdm";
    displayManager.lightdm = {
      enable = true;
    };
    layout = "us";
    libinput.enable = true;
    videoDrivers = [ "intel" "nvidia" ];
    windowManager.bspwm.enable = true;
  };

  services.connman = {
    enable = true;
    extraConfig = "
      [General]
      AllowHostnameUpdates=false
    ";
    networkInterfaceBlacklist = [ "vmnet" "vboxnet" "virbr" "ifb" "docker" "veth" ];
  };

  # TODO see crontab service
  services.docker.enable = true;
  services.tlp = {
    enable = true;
    # TODO set extraConfig tlp
  };

  services.thermald = {
    enable = true;
    # TODO set extraConfig thermald
  };

  services.fstrim = {
    enable = true;
    interval = "weekly";
  };
}
