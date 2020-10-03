{ config, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    autorun = true;
    desktopManager.default = null;
    displayManager.lightdm = {
      enable = true;
    };
    layout = "us";
    libinput.enable = true;
    extraConfig = ''
      Section "InputClass"
        Identifier "touchpad"
        Driver "libinput"
        MatchIsTouchpad "on"
        Option "NaturalScrolling" "true"
      EndSection
    '';
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
    wifi.backend = "iwd";
  };

  # TODO see crontab service
  services.tlp = {
    enable = true;
    extraConfig = "TLP_ENABLE=1\n
TLP_DEFAULT_MODE=AC\n
WIFI_PWR_ON_AC=off\n
WIFI_PWR_ON_BAT=on\n
CPU_HWP_ON_AC=performance\n
CPU_HWP_ON_BAT=balance-performance\n
DEVICES_TO_ENABLE_ON_STARTUP=\"bluetooth wifi\"\n";
  };

  services.thermald = {
    enable = true;
  };

  services.fstrim = {
    enable = true;
    interval = "weekly";
  };

  services.postgresql.enable = true;

  # systemd.user.services."betterlockscreen" = {
  #   description = "Lock screen when going to sleep/suspend";
  #   before = [ "sleep.target" "suspend.target" ];
  #   environment = {
  #     "DISPLAY"= ":0";
  #   };
  #   script = "${pkgs.betterlockscreen}/bin/betterlockscreen --lock" ;
  #   postStart = "/sbin/sleep 1";
  #   wantedBy = [ "sleep.target" "suspend.target" ];
  #   enable = true;
  # };
    systemd.user.services.betterlockscreen = {
    enable = true;
    description = "Locks screen when going to sleep/suspend";
    environment = { DISPLAY = "0"; };
    serviceConfig = {
      User = "fedeizzo";
      Type = "simple";
      ExecStart = ''${pkgs.betterlockscreen}/bin/betterlockscreen --lock'';
      TimeoutSec = "infinity";
    };
    wantedBy = [ "sleep.target" "suspend.target" ];
    before = [ "sleep.target" "suspend.target" ];
    aliases = [ "betterlockscreen@fedeizzo.service" ];
  };
}
