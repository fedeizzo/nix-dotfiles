{ pkgs, ... }:
let
  sources = import ../nix/sources.nix;
in
{
  imports = [
    ../modules/alacritty.nix
    ../modules/chat.nix
    ../modules/cli.nix
    ../modules/cuda.nix
    ../modules/languages.nix
    ../modules/media.nix
    ../modules/neovim.nix
    ../modules/nixos-desktop.nix
    ../modules/services.nix
    ../modules/ssh.nix
  ];

  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";
  home.stateVersion = "20.09";

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableAutosuggestions = true;
    history.save = 10000;
    plugins = [{
      name = "zsh-history-substring-search";
      src = pkgs.fetchFromGitHub {
        inherit (sources.historysubstring) owner repo rev sha256;
      };
    }
      {
        name = "zsh-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          inherit (sources.syntaxhighlighting) owner repo rev sha256;
        };
      }];
    oh-my-zsh = {
      enable = true;
    };
    initExtra = builtins.readFile ../../dotfiles/dot_zshrc;
    envExtra = builtins.readFile ../../dotfiles/dot_zshenv;
  };

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = "org.pwmt.zathura.desktop";
      "inode/directory" = "lf.desktop";
      "text/plain" = "nvim.desktop";
      "text/html" = "org.qutebrowser.qutebrowser.desktop";
    };
  };
}
