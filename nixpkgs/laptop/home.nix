{ pkgs, ... }:
let
  sources = import ../../nix/sources.nix;
in
{
  imports = [
    ../../modules/alacritty.nix
    ../../modules/chat.nix
    ../../modules/cli.nix
    ../../modules/cuda.nix
    ../../modules/languages.nix
    ../../modules/media.nix
    ../../modules/neovim.nix
    ../../modules/nix-utilities.nix
    ../../modules/nixos-desktop.nix
    ../../modules/ssh.nix
  ];

  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";
  home.stateVersion = "20.09";

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
    histSize = 10000;
    plugins = [{
      name = "zsh-history-substring-search";
    }]
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" ];
    }
  };

  # TODO see how lorri works
  # services.lorri.enable = true;
}
