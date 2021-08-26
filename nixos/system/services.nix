{ config, pkgs, ... }:

{
  #################################
  # SERVICES
  #################################
  services.xserver = {
    enable = true;
    autorun = true;
    desktopManager.default = null;
    displayManager.lightdm = {
      enable = true;
    };
    layout = "us";
    xkbVariant = "altgr-intl";
    libinput.enable = true;
    extraConfig = ''
      Section "InputClass"
        Identifier "touchpad"
        Driver "libinput"
        MatchIsTouchpad "on"
        Option "NaturalScrolling" "true"
      EndSection
    '';

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };
  services.tlp = {
    enable = true;
    settings = {
      TLP_ENABLE = 1;
      TLP_DEFAULT_MODE = "AC";
      WIFI_PWR_ON_AC = "off";
      WIFI_PWR_ON_BAT = "on";
      CPU_HWP_ON_AC = "performance";
      CPU_HWP_ON_BAT = "performance";
      DEVICES_TO_ENABLE_ON_STARTUP = "bluetooth wifi";
    };
  };
  services.thermald = {
    enable = true;
    # dell
    # configFile = TODO: add path to generated config;
  };
  services.fstrim = {
    enable = true;
    interval = "weekly";
  };
  xdg.portal.enable = true;
  users.groups = {
    steamps4 = { };
  };
  services.upower = {
    enable = true;
    usePercentageForPolicy = false;
  };
  services.gnome.gnome-keyring.enable = true;
  security.pam.services.lightdm.enableGnomeKeyring = true;
  security.pam.services.lightdm-greeters.enableGnomeKeyring = true;
  security.pam.services.login.fprintAuth = true;
  security.pam.services.system-local-login.fprintAuth = true;
  security.pam.services.lightdm.fprintAuth = true;
  security.pam.services.doas.fprintAuth = true;
  services.fprintd = {
    enable = true;
    tod = {
      enable = true;
      driver = pkgs.libfprint-2-tod1-goodix;
    };
  };
  # systemd.services.autorandr = {
  #     description = "autorandr execution hook";
  #     after = [ "sleep.target" ];
  #     startLimitIntervalSec = 5;
  #     startLimitBurst = 1;
  #     serviceConfig = {
  #       ExecStart = "${pkgs.autorandr}/bin/autorandr --batch --change --default default";
  #       Type = "oneshot";
  #       RemainAfterExit = false;
  #       KillMode = "process";
  #     };
  #     wantedBy = [ "sleep.target" ];
  # };
}
