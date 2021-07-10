{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    neovide
  ];
  programs.neovim = {
    enable = true;
    package = pkgs.neovim;
    withNodeJs = false;
    withPython3 = true;
    withRuby = false;
  };
  home.file.".config/nvim" = {
     source = ../dotfiles/nvim;
     executable = false;
     recursive = true;
  };
}
