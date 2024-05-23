{ pkgs, ... }:

{
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
  };

  sound = {
    enable = false;
    mediaKeys.enable = false;
  };

  location.latitude = 41.0;
  location.longitude = 12.0;
  services.clight = {
    enable = true;
  };

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    socketActivation = true;
    audio.enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    wireplumber.enable = true;
  };

  programs.light.enable = true;
}
