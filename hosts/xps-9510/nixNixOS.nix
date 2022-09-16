{ config, pkgs, inputs, ... }:

{
  nixpkgs = {
    config = {
      allowUnfree = true;
      joypixels.acceptLicense = true;
    };
  };
  system.stateVersion = "22.05";
  nix = {
    settings.auto-optimise-store = true;
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
      persistent = true;
    };
  };
}
