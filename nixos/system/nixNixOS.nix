{ config, pkgs, ... }:

{
  nixpkgs.config = {
    allowUnfree = true;
  };
  system.stateVersion = "21.11";
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
