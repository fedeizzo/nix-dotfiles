{ config, username, lib, pkgs, ... }:

{
  mobile.beautification = {
    silentBoot = true;
    splash = true;
  };
  services.xserver.desktopManager.phosh = {
    user = "${username}";
    enable = true;
    group = "users";
  };
  hardware.sensor.iio.enable = true;
}
