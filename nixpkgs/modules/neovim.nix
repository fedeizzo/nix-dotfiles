{ config, pkgs, libs, ... }:

{
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-nightly;
    extraConfig = builtins.readFile ../dotfiles/dot_config/nvim/init.vim;
    withNodeJs = false;
    withPython3 = false;
    withRuby = false;
  };

  home.file.".config/nvim/lua" = {
    source = ../dotfiles/dot_config/nvim/lua;
    executable = true;
    recursive = true;
  };
  home.file.".config/nvim/snippet" = {
    source = ../dotfiles/dot_config/nvim/snippet;
    executable = true;
    recursive = true;
  };
}
