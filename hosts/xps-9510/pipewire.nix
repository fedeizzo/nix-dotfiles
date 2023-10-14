{ config, lib, pkgs, nixpkgs-unstable, nixpkgs-old, ... }:

{
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
}
