{ inputs, ... }:

let
  inherit (inputs) self home-manager sops nixpkgs deploy-rs;
  inherit (self) outputs;
  inherit (nixpkgs.lib) nixosSystem genAttrs;
in
rec {
  systems = [
    # "aarch64-darwin"
    "aarch64-linux"
    # "i686-linux"
    # "x86_64-darwin"
    "x86_64-linux"
  ];
  forAllSystems = genAttrs systems;

  dockerNetworkScript = { dockerBin, networkName }:
    ''
      ${dockerBin} network inspect ${networkName} >/dev/null 2>&1 || ${dockerBin} network create --driver bridge ${networkName}
    '';

  mkHost =
    { username
    , syncthing
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
          config.allowUnfree = true;
          config.joypixels.acceptLicense = true;
          overlays = builtins.attrValues { emacs = inputs.emacs-overlay.overlays.default; };
        };
        _module.args.nixpkgs-old = import inputs.nixpkgs-old {
          inherit (pkgs.stdenv.targetPlatform) system;
          config.allowUnfree = true;
          config.joypixels.acceptLicense = true;
        };
      };
    in
    nixosSystem
      {
        pkgs = if machine != "duet" then pkgs else null;
        inherit system;
        specialArgs = {
          inherit inputs username hostname fs syncthing dockerNetworkScript;
          mobile-nixos = inputs.mobile-nixos;
        };

        modules = [
          defaults
          # (inputs.nix-bubblewrap.lib { inherit system pkgs; })
          ../hosts/${machine}
          ../home/${machine}
        ];
      };
}
