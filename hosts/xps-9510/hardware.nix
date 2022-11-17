{ config, lib, pkgs, nixpkgs-unstable, nixpkgs-old, ... }:

{
  powerManagement.enable = true;
  hardware = {
    opengl = {
      driSupport32Bit = true;
      enable = true;
      extraPackages = [
        pkgs.intel-media-driver
        pkgs.vaapiIntel
        pkgs.vaapiVdpau
        pkgs.libvdpau-va-gl
      ];
    };
    bluetooth = {
      enable = true;
      powerOnBoot = false;
      disabledPlugins = [ "sap" ];
    };
    cpu.intel.updateMicrocode = true;
  };
  sound = {
    enable = false;
    mediaKeys.enable = false;
  };
  services = {
    smartd = {
      enable = true;
      autodetect = true;
      notifications.x11.enable = true;
    };
    # keyboard daemon for remapping
    tlp = {
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
    thermald = {
      enable = true;
      # dell
      # configFile = TODO: add path to generated config;
    };
    fstrim = {
      enable = true;
      interval = "weekly";
    };
    fprintd = {
      enable = true;
      package = nixpkgs-old.fprintd;
      tod = {
        enable = true;
        driver = pkgs.libfprint-2-tod1-goodix;
      };
    };
    auto-cpufreq.enable = true;
    upower = {
      enable = true;
      usePercentageForPolicy = false;
    };
  };
  systemd.services = {
    seatd = {
      enable = true;
      description = "Seat management daemon";
      script = "${pkgs.seatd}/bin/seatd -g wheel";
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "1";
      };
      wantedBy = [ "multi-user.target" ];
    };
    keyd = {
      enable = true;
      description = "key remapping daemon";
      script = "${nixpkgs-unstable.keyd}/bin/keyd";
      requires = [ "local-fs.target" ];
      after = [ "local-fs.target" ];
      serviceConfig = {
        Type = "simple";
      };
      wantedBy = [ "sysinit.target" ];
    };
  };
  environment.etc = {
    keyd = {
      target = "keyd/laptop-keyboard.conf";
      text = ''
        [ids]
        0001:0001
        [main]
        capslock = overload(control, leftcontrol)
        leftcontrol = capslock
      '';
    };
  };
  location.latitude = 41.0;
  location.longitude = 12.0;
  services.clight = {
    enable = true;
  };
}
