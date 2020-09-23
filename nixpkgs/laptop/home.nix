{ pkgs, ... }:
let
  sources = import ../../nix/sources.nix;
in
{
  imports = [
    ../modules/alacritty.nix
    ../modules/chat.nix
    ../modules/cli.nix
    #../modules/cuda.nix
    ../modules/languages.nix
    ../modules/media.nix
    ../modules/neovim.nix
    ../modules/nix-utilities.nix
    ../modules/nixos-desktop.nix
    ../modules/ssh.nix
  ];

  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";
  home.stateVersion = "20.09";

  programs.zsh = {
    enable = true;
    # TODO in order to enable completion
    # environment.pathsToLink = [ "/share/zsh" ];
    enableCompletion = true;
    enableAutosuggestions = true;
    #syntaxHighlighting.enable = true;
    history.save = 10000;
    initExtraBeforeCompInit = "export ZSH=${pkgs.oh-my-zsh}/share/oh-my-zsh/";
    #plugins = [{
    #  name = "zsh-history-substring-search";
    #}];
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" ];
    };
  };

  # TODO see how lorri works
  # services.lorri.enable = true;
  home.file.".zshrc".source = ../../dotfiles/dot_zshrc;
  home.file.".zshenv".source = ../../dotfiles/dot_zshenv;
  home.file.".sources" = {
    source = ../../dotfiles/dot_sources;
    executable = true;
    recursive = true;
  };
  home.file."./.config/bspwm/bspwmrc" = {
    source = ../../dotfiles/dot_config/bspwm/executable_bspwmrc;
    executable = true;
  };
  xdg.configFile."sxhkd/sxhkdrc".source = ../../dotfiles/dot_config/sxhkd/sxhkdrc;
}
