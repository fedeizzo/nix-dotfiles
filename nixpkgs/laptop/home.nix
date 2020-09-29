{ pkgs, ... }:
let
  sources = import ../nix/sources.nix;
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
    }
    ];
    oh-my-zsh = {
      enable = true;
    };
    initExtra = builtins.readFile ../../dotfiles/dot_zshrc;
    envExtra = builtins.readFile ../../dotfiles/dot_zshenv;
  };

  home.file.".sources" = {
    source = ../../sources;
    executable = true;
    recursive = true;
  };
  home.file."./.config/bspwm/bspwmrc" = {
    source = ../../dotfiles/dot_config/bspwm/executable_bspwmrc;
    executable = true;
  };
  home.file.".config/nvim" = {
    source = ../../dotfiles/dot_config/nvim;
    executable = true;
    recursive = true;
  };
  home.file.".gitconfig" = {
    source = ../../dotfiles/dot_gitconfig;
  };
  xdg.configFile."sxhkd/sxhkdrc".source = ../../dotfiles/dot_config/sxhkd/sxhkdrc;
  xdg.configFile."rofi/config.rasi".source = ../../dotfiles/dot_config/rofi/config.rasi;

  services.lorri.enable = true;
}
