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
    #videoDrivers = [ "intel" "nvidia" ];
    videoDrivers = [ "intel" ];
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
    # settings = {
    #   "TLP_ENABLE" = 1;
    #   "TLP_DEFAULT_MODE" = "AC";
    #   "WIFI_PWR_ON_AC" = "off";
    #   "WIFI_PWR_ON_BAT" = "on";
    #   "CPU_HWP_ON_AC" = "performance";
    #   "CPU_HWP_ON_BAT" = "balance-performance";
    # };
    extraConfig = "TLP_ENABLE = 1\n
TLP_DEFAULT_MODE = AC\n
WIFI_PWR_ON_AC = off\n
WIFI_PWR_ON_BAT = on\n
CPU_HWP_ON_AC = performance\n
CPU_HWP_ON_BAT = balance-performance\n";
  };

  services.thermald = {
    enable = true;
  };

  services.fstrim = {
    enable = true;
    interval = "weekly";
  };

  systemd.user.services."betterlockscreen" = {
    description = "Lock screen when going to sleep/suspend";
    before = [ "sleep.target" "suspend.target" ];
    environment = {
      "DISPLAY"= ":0";
    };
    script = "/usr/bin/betterlockscreen --lock" ;
    postStart = "/sbin/sleep 1";
    wantedBy = [ "sleep.target" "suspend.target" ];
    enable = true;
  };
}
