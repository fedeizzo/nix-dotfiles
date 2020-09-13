{ config, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    autorun = true;
    desktopManager.default = null;
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
  services.tlp = {
    enable = true;
    # Settings https://linrunner.de/tlp
    settings = {
      "TLP_ENABLE" = 1;
      "TLP_DEFAULT_MODE" = "AC";
      "WIFI_PWR_ON_AC" = "off";
      "WIFI_PWR_ON_BAT" = "on";
      "CPU_HWP_ON_AC" = "performance";
      "CPU_HWP_ON_BAT" = "balance-performance";
    };
  };

  services.thermald = {
    enable = true;
  };

  services.fstrim = {
    enable = true;
    interval = "weekly";
  };
}
