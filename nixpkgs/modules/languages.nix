{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    python3
    clang
    lua
    
    # lua support neovim
    bat
    ripgrep
    nodePackages.bash-language-server
    nodePackages.dockerfile-language-server-nodejs
    python38Packages.pyls-black
    python38Packages.python-language-server
    ccls
  ];
  # TODO work
  # qgis
  # gdal
  # postgres
  # postgis
}
