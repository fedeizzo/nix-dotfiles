{ pkgs, pkgs-unstable, system-overlays, inputs, ... }:

{
  system.stateVersion = "24.11";

  nixpkgs = {
    overlays = builtins.attrValues system-overlays ++ [
      (_:_: {
        inherit (pkgs-unstable) rbw;
      })
      inputs.nix-topology.overlays.default
    ];
    config.allowUnfree = true;
    config.joypixels.acceptLicense = true;
  };

  nix = {
    settings = {
      auto-optimise-store = true;
      substituters = [
        "https://nix-community.cachix.org"
        "https://cache.nixos.org/"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
    package = pkgs.nixVersions.stable;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 10d";
      persistent = true;
    };
  };
}
