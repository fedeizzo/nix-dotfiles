{ config, pkgs, ... }:

{
  # TODO maybe useful for Italian accent
  # services.xserver.xkbOptions = "eurosign:e";
  # TODO this 32bit support is useful only with steam
  # hardware.opengl.driSupport32Bit = true;
  # hardware.pulseaudio.support32Bit = true;

  hardware.opengl = {
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
  };

  hardware.bumblebee = {
    enable = true;
    driver = "nvidia";
  };

  hardware.cpu.intel.updateMicrocode = true;

  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    extraConfig = "load-module module-bluetooth-discover a2dp_config=\"ldac_eqmid=sq\"\n";
  };
}
