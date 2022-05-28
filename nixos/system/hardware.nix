{ config, lib, pkgs, nixpkgs-unstable, ... }:

{
  powerManagement.enable = true;
  hardware.opengl = {
    driSupport32Bit = true;
    enable = true;
    extraPackages = [
      pkgs.intel-media-driver
      pkgs.vaapiIntel
      pkgs.vaapiVdpau
      pkgs.libvdpau-va-gl
    ];
  };
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    disabledPlugins = [ "sap" ];
  };
  hardware.cpu.intel.updateMicrocode = true;
  sound = {
    enable = false;
    mediaKeys.enable = false;
  };
  services.smartd = {
    enable = true;
    autodetect = true;
    notifications.x11.enable = true;
  };
}
