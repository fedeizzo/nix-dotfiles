{ pkgs, ... }:

{
  hardware = {
    graphics = {
      enable32Bit = true;
      enable = true;
      extraPackages = [
        pkgs.intel-media-driver
        pkgs.libva-vdpau-driver
        pkgs.libvdpau-va-gl
      ];
    };
  };
  environment.sessionVariables = { LIBVA_DRIVER_NAME = "iHD"; }; # Force intel-media-driver

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
