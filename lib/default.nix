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

  kubernetesOrderString = { intOrder }:
    let
      requirePadding = if intOrder < 10 then true else false;
      strOrder = (toString intOrder);
      order = if requirePadding then "0" + strOrder else strOrder;
    in
    order;

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
      specialArgs = { inherit inputs username hostname fs kubernetesOrderString; };
      modules = [
        defaults
        # (inputs.nix-bubblewrap.lib { inherit system pkgs; })
        ../hosts/${machine}
        ../home/${machine}
      ];
    };
}
