{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = [
    pkgs.avrdude
    pkgs.python3
    pkgs.python3Packages.pillow
  ];
}
