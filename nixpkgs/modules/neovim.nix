{ config, pkgs, libs, ... }:

{
  # TODO install all lsp
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-nightly;
    configure = {
      customRC = builtins.readFile ../../dotfiles/dot_config/nvim/init.vim;
      plug.plugins = with pkgs.vimPlugins; [
      ];
    };
    withNodeJs = false;
    withPython = false;
    withPython3 = false;
    withRuby = false;
  };

  home.file.".config/nvim" = {
    source = ../../dotfiles/dot_config/nvim;
    executable = true;
    recursive = true;
  };
}
