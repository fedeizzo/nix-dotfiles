{ config, username, lib, pkgs, ... }:

{
  mobile.boot.stage-1.networking.enable = lib.mkDefault true;
  mobile.beautification = {
    silentBoot = lib.mkDefault true;
    splash = lib.mkDefault true;
  };

  services.xserver = {
    enable = true;

    desktopManager.plasma5.mobile.enable = true;
    desktopManager.plasma5.mobile.installRecommendedSoftware = false;

    displayManager.autoLogin = {
      enable = true;
      user = "${username}";
    };

    displayManager.defaultSession = "plasma-mobile";

    displayManager.lightdm = {
      enable = true;
      extraSeatDefaults = ''
        session-cleanup-script=${pkgs.procps}/bin/pkill -P1 -fx ${pkgs.lightdm}/sbin/lightdm
      '';
    };

    libinput.enable = true;
  };

  hardware.bluetooth.enable = true;
  hardware.pulseaudio.enable = lib.mkDefault true; # mkDefault to help out users wanting pipewire
  networking.networkmanager.enable = true;
  networking.wireless.enable = false;
  networking.networkmanager.unmanaged = [ "rndis0" "usb0" ];
  powerManagement.enable = true;
  hardware.sensor.iio.enable = true;
}
