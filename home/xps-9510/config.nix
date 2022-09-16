{ pkgs }:

{
  allowUnfree = true;
  permittedInsecurePackages = [
    "electron-13.6.9"
  ];
}
