{ config, lib, pkgs, nixpkgs-unstable, ... }:

{
  #################################
  # HARDWARE
  #################################
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
  # hardware.pulseaudio = {
  #   support32Bit = true;
  #   enable = true;
  #   extraModules = [ pkgs.pulseaudio-modules-bt ];
  #   extraConfig = "load-module module-bluetooth-discover a2dp_config=\"ldac_eqmid=sq\"\n";
  #   package = pkgs.pulseaudioFull;
  # };
  sound = {
    enable = false;
    mediaKeys.enable = false;
  };
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    package = nixpkgs-unstable.pipewire;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    media-session = {
      enable = true;
      package = nixpkgs-unstable.pipewire.mediaSession;
    };
  };
  services.smartd = {
    enable = true;
    autodetect = true;
    notifications.x11.enable = true;
  };
}
