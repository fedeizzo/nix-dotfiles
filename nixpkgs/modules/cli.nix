{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    coreutils
    exa
    fd
    fzf
    gawk
    gnused
    gnutls
    htop
    powertop
    starship
    nix-zsh-completions
    nmap
    lf
    universal-ctags
    unzip
    xsv
    gotop
    zsh
    zsh-history-substring-search
  ];


  programs.direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
  };

  home.file.".gitconfig".source = ../../dotfiles/dot_gitconfig;
  # xdg.configFile."direnv/lib/poetry.sh".source = ../configs/direnv/poetry.sh;
}

