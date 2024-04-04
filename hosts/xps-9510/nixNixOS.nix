{ config, nixpkgs-unstable, inputs, ... }:

{
  # nixpkgs = {
  #   config = {
  #     allowUnfree = true;
  #     joypixels.acceptLicense = true;
  #   };
  # };
  system.stateVersion = "23.11";
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
    package = nixpkgs-unstable.nixVersions.nix_2_19;
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
