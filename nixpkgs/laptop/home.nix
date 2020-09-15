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
    ../../modules/git.nix
    ../../modules/media.nix
    ../../modules/nix-utilities.nix
    ../../modules/nixos-desktop.nix
    ../../modules/python.nix
    ../../modules/ssh.nix
    ../../modules/languages.nix
    ../../modules/neovim.nix
    ../../modules/emacs.nix
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
      src = pkgs.fetchFromGitHub {
        inherit (sources.zsh-history-substring-search) owner repo rev sha256;
      };
    }]
  };

  # TODO see how lorri works
  services.lorri.enable = true;
}
