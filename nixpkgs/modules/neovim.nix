{ config, pkgs, libs, ... }:

{
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-nightly;
    extraConfig = builtins.readFile ../dotfiles/dot_config/nvim/init.vim;
    withNodeJs = false;
    withPython = true;
    withPython3 = true;
    withRuby = false;
  };

  home.file.".config/nvim/customScript" = {
    source = ../dotfiles/dot_config/nvim/customScript;
    executable = true;
    recursive = true;
  };
  home.file.".config/nvim/lua" = {
    source = ../dotfiles/dot_config/nvim/lua;
    executable = true;
    recursive = true;
  };
  home.file.".config/nvim/plugins" = {
    source = ../dotfiles/dot_config/nvim/plugins;
    executable = true;
    recursive = true;
  };
  home.file.".config/nvim/snippet" = {
    source = ../dotfiles/dot_config/nvim/snippet;
    executable = true;
    recursive = true;
  };
  home.file.".config/nvim/templates" = {
    source = ../dotfiles/dot_config/nvim/templates;
    executable = true;
    recursive = true;
  };
}
