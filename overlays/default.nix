{ inputs, ... }:
let
  # Pass flake inputs to overlay so we can use the sources pinned in flake.lock
  # instead of having to keep sha256 hashes in each package for src
  additions = import ./pkgs;
  modifications = _self: _super: {
    swayosd = _super.swayosd.overrideAttrs
      (_: rec {
        version = "v0.1.0";
        src = _self.fetchFromGitHub {
          owner = "ErikReider";
          repo = "SwayOSD";
          rev = version;
          hash = "sha256-GyvRWEzTxQxTAk+xCLFsHdd1SttBliOgJ6eZqAxQMME=";
        };
        cargoDeps = _self.rustPlatform.fetchCargoTarball {
          inherit src;
          name = "swayosd-${version}";
          hash = "sha256-Tvalky7EDyJhwT4dJ8i85/QKpCVKGpb6y5EIRKygMXs=";
        };
      });
  };
in
inputs.nixpkgs.lib.composeManyExtensions [ modifications additions ]
