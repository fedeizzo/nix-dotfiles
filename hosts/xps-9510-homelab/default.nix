{ inputs, ... }:

{
  imports = [
    inputs.nix-topology.nixosModules.default
    inputs.authentik-nix.nixosModules.default

    ./services
    ./system/backup.nix
    ./system/boot.nix
    ./system/deployment.nix
    ./system/disko.nix
    ./system/hardware.nix
    ./system/misc.nix
    ./system/networking.nix
    ./system/nix.nix
    ./system/security.nix
    ./system/sops.nix
    ./system/users.nix
  ];
}
