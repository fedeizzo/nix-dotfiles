{ inputs, ... }:

let
  inherit (inputs) nixpkgs;
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

  mkHost =
    { username
    , hostname
    , system
    , machine
    , pkgs
    , isMac ? false
    }:
    if isMac
    then
      inputs.nix-darwin.lib.darwinSystem
        {
          pkgs = pkgs;
          inherit system;
          specialArgs = {
            inherit inputs username hostname;
          };

          modules = [
            inputs.home-manager.darwinModules.home-manager
            # inputs.nh-darwin.nixDarwinModules.default

            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = {
                  inherit username inputs;
                };

                users.${username} = import ../home/${machine};
              };
            }
            ../hosts/${machine}
          ];
        }
    else
      nixosSystem
        {
          pkgs = pkgs;
          inherit system;
          specialArgs = {
            inherit inputs username hostname;
          };

          modules = [
            ../hosts/${machine}
            ../home/${machine}
          ];
        };
}
