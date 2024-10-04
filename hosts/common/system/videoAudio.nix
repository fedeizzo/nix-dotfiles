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

  location.latitude = 48.8575;
  location.longitude = 2.3514;

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
