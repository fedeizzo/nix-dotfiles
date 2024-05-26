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
    , hostname
    , system
    , machine
    , pkgs
    }:
    let
      config = {
        inherit username hostname;
      };
    in
    nixosSystem
      {
        pkgs = pkgs;
        inherit system;
        specialArgs = {
          inherit inputs username hostname dockerNetworkScript;
        };

        modules = [
          ../hosts/${machine}
          ../home/${machine}
        ];
      };
}
