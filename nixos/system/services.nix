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
    # dell: move intel to nvidia
    # videoDrivers = [ "intel" ];
    videoDrivers = [ "nvidia" ];

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
  services.udev.extraRules = with pkgs; ''
    # This rule is needed for basic functionality of the controller in Steam and keyboard/mouse emulation
    SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", MODE="0666"

    # This rule is necessary for gamepad emulation; make sure you replace 'pgriffais' with a group that the user that runs Steam belongs to
    KERNEL=="uinput", MODE="0660", GROUP="steamps4", OPTIONS+="static_node=uinput"

    # Valve HID devices over USB hidraw
    KERNEL=="hidraw*", ATTRS{idVendor}=="28de", MODE="0666"

    # Valve HID devices over bluetooth hidraw
    KERNEL=="hidraw*", KERNELS=="*28DE:*", MODE="0666"

    # DualShock 4 over USB hidraw
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="05c4", MODE="0666"

    # DualShock 4 wireless adapter over USB hidraw
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="0ba0", MODE="0666"

    # DualShock 4 Slim over USB hidraw
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="09cc", MODE="0666"

    # DualShock 4 over bluetooth hidraw
    KERNEL=="hidraw*", KERNELS=="*054C:05C4*", MODE="0666"

    # DualShock 4 Slim over bluetooth hidraw
    KERNEL=="hidraw*", KERNELS=="*054C:09CC*", MODE="0666"
  '';
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
