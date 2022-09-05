{ pkgs }:

{
  allowUnfree = true;
  joypixels.acceptLicense = true;
  # packageOverrides = pkgs: with pkgs; rec {
  #   swayhide = callPackage ../pkgs/swayhide.nix { };
  #   swaync = callPackage ../pkgs/swaync.nix { };
  # };
  permittedInsecurePackages = [
    "electron-13.6.9"
  ];
}
