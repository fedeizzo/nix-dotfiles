{ config, pkgs, libs, ... }:

{
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-nightly;
    extraConfig = builtins.readFile ../dotfiles/nvim/init.vim;
    withNodeJs = false;
    withPython3 = true;
    withRuby = false;
  };

  home.file.".config/nvim/lua" = {
    source = ../dotfiles/nvim/lua;
    executable = true;
    recursive = true;
  };
  home.file.".config/nvim/snippet" = {
    source = ../dotfiles/nvim/snippet;
    executable = true;
    recursive = true;
  };
}
