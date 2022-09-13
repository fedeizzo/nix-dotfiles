{ pkgs }:

{
  allowUnfree = true;
  joypixels.acceptLicense = true;
  permittedInsecurePackages = [
    "electron-13.6.9"
  ];
}
