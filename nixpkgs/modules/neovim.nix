{ config, pkgs, libs, ... }:

{
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-nightly;
    configure = {
      customRC = builtins.readFile ../dotfiles/dot_config/nvim/init.vim;
      plug.plugins = with pkgs.vimPlugins; [
      ];
    };
    withNodeJs = false;
    withPython = true;
    withPython3 = true;
    withRuby = false;
  };

  home.file.".config/nvim" = {
    source = ../dotfiles/dot_config/nvim;
    executable = true;
    recursive = true;
  };
}
