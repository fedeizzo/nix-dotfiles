{ pkgs, pkgs-unstable, system-overlays, inputs, ... }:

{
  system.stateVersion = "25.05";

  nixpkgs = {
    overlays = builtins.attrValues system-overlays ++ [
      (final: prev: {
        inherit (pkgs-unstable) rbw;
      })
      (
        final: prev: {
          anytype = pkgs-unstable.anytype.overrideAttrs rec {
            version = "0.46.1";
            src = final.fetchFromGitHub {
              owner = "anyproto";
              repo = "anytype-ts";
              tag = "v${version}";
              hash = "sha256-S8X8NTj5LCGoWasRbYZ3gdDoi1oi6/Ji/hYH1+q7HMc=";
            };

            locales = final.fetchFromGitHub {
              owner = "anyproto";
              repo = "l10n-anytype-ts";
              rev = "89343848ff6e2bc655afa3c5a1d905fbc47197c5";
              hash = "sha256-Px6orAg7Lf05XXHKTznMwu/AlLsV23i+CdKT0T7r2Iw=";
            };
            npmDepsHash = "sha256-pmPg/idU3RK9TtaKy2AE/rYYumIUMd/EG01/QPq2aPg=";
            npmDeps = final.fetchNpmDeps {
              inherit src;
              name = "${final.anytype.pname}-${version}-npm-deps";
              hash = npmDepsHash;
            };
          };
        }
      )
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
    package = pkgs.nixVersions.nix_2_29;
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
