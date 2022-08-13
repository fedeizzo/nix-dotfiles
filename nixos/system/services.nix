{ config, pkgs, nixpkgs-old, ... }:

let
  hdmiEventHandler = pkgs.writeShellScriptBin "hdmiEventHandler" (builtins.readFile ../bin/hdmiEventHandler.sh);
in
{
  #################################
  # SERVICES
  #################################
  services.xserver = {
    enable = false;
    autorun = false;
    desktopManager.default = null;
    displayManager.lightdm.enable = false;
    displayManager.sddm.enable = false;
    displayManager.gdm.enable = false;
    layout = "us";
    xkbVariant = "altgr-intl";
    libinput = {
      enable = true;

      touchpad = {
        tapping = true;
        horizontalScrolling = true;
        scrollMethod = "twofinger";
        naturalScrolling = true;
        disableWhileTyping = true;
      };
    };
    xkbOptions = "ctrl:swapcaps";
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
  users.groups = {
    steamps4 = { };
  };
  services.upower = {
    enable = true;
    usePercentageForPolicy = false;
  };
  services.gnome.gnome-keyring.enable = false;
  services.flatpak.enable = true;
  services.fprintd = {
    enable = true;
    package = nixpkgs-old.fprintd;
    tod = {
      enable = true;
      driver = pkgs.libfprint-2-tod1-goodix;
    };
  };
  environment.systemPackages = [
    hdmiEventHandler
    pkgs.xorg.xrandr
  ];
  services.tailscale.enable = true;
  # services.emacs = {
  #   package = pkgs.emacsPgtkGcc;
  #   enable = true;
  # };
}
