{ inputs, ... }:
let
  # Pass flake inputs to overlay so we can use the sources pinned in flake.lock
  # instead of having to keep sha256 hashes in each package for src
  additions = import ../pkgs;
  modifications = self: super: {
    waybar = super.waybar.overrideAttrs (oldAttrs: {
      mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
    });
    hyprland = super.hyprland.overrideAttrs (oldAttrs: {
      patches = [
        (self.fetchpatch {
          url = "https://github.com/hyprwm/Hyprland/pull/6136.patch";
          hash = "sha256-KAkuogPQeA+ZM5Fkmw3Tdu5GRaLqDVv/s7mHVACI6Ho=";
        })
      ];
    });
  };
in
inputs.nixpkgs.lib.composeManyExtensions [ modifications additions ]
