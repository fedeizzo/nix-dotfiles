{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    python39
    clang
    ctags
    lua
  ];
  # TODO work
  # qgis
  # gdal
  # postgres
  # postgis
}
