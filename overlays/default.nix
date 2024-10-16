{ inputs, ... }:
let
  # Pass flake inputs to overlay so we can use the sources pinned in flake.lock
  # instead of having to keep sha256 hashes in each package for src
  additions = import ./pkgs;
  modifications = _self: _super: {
    # waybar = super.waybar.overrideAttrs (oldAttrs: {
    #   mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
    # });
  };
in
inputs.nixpkgs.lib.composeManyExtensions [ modifications additions ]
