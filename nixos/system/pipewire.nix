{ config, lib, pkgs, nixpkgs-unstable, ... }:

{
  security.rtkit.enable = true;
  services.pipewire  = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    media-session.enable = true;
  };
}
