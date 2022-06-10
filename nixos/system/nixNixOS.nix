{ config, pkgs, ... }:

{
  nixpkgs.config = {
    allowUnfree = true;
  };
  system.stateVersion = "22.05";
  nix = {
    autoOptimiseStore = true;
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    binaryCaches = [
    ];
    binaryCachePublicKeys = [
    ];
  };
}
