{ pkgs, pkgs-unstable, system-overlays, inputs, ... }:

{
  system.stateVersion = "25.05";

  nixpkgs = {
    overlays = builtins.attrValues system-overlays ++ [
      (final: prev: {
        inherit (pkgs-unstable) rbw;
      })
      inputs.nix-topology.overlays.default
    ];
    config.allowUnfree = true;
    config.joypixels.acceptLicense = true;
  };

  nix = {
    settings = {
      trusted-users = [ "root" "nixremote" "@wheel" ];
      auto-optimise-store = true;
      substituters = [
        "https://nix-community.cachix.org"
        "https://cache.nixos.org/"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
    # handled by nh
    # gc = {
    #   automatic = true;
    #   dates = "weekly";
    #   options = "--delete-older-than 10d";
    #   persistent = true;
    # };
  };
}
