{ inputs, ... }:

let
  inherit (inputs) self home-manager sops nixpkgs deploy-rs;
  inherit (self) outputs;
  inherit (nixpkgs.lib) nixosSystem genAttrs;
in
rec {
  systems = [
    "aarch64-darwin"
    "aarch64-linux"
    "i686-linux"
    "x86_64-darwin"
    "x86_64-linux"
  ];
  forAllSystems = genAttrs systems;

  mkHost =
    { username
    , hostname
    , fs
    , system
    , machine
    , pkgs
    }:
    let
      config = {
        inherit username hostname fs;
      };
      defaults = { pkgs, ... }: {
        _module.args.nixpkgs-unstable = import inputs.nixpkgs-unstable {
          inherit (pkgs.stdenv.targetPlatform) system;
        };
        _module.args.nixpkgs-old = import inputs.nixpkgs-old {
          inherit (pkgs.stdenv.targetPlatform) system;
        };
      };
    in
    nixosSystem {
      inherit pkgs system;
      specialArgs = { inherit inputs username hostname fs; };
      modules = [
        defaults
        ../hosts/${machine}
        ../home/${machine}
      ];
    };
}
