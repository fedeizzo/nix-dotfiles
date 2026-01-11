{ ... }:

{
  imports = [
    ./system/backup.nix
    ./system/boot.nix
    ./system/disko.nix
    ./system/hardware.nix
    ./system/misc.nix
    ./system/networking.nix
    ./system/nix.nix
    ./system/security.nix
    # ./system/sops.nix
    ./system/users.nix
  ];
}
