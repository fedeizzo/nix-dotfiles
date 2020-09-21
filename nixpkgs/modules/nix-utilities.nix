{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    niv
    nixpkgs-fmt
    nixpkgs-lint
  ];
}

