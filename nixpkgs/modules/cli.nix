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
    dragon-drop
    unzip
    xsv
    gotop
    zsh
  ];


  programs.direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
  };

  home.file.".gitconfig" = {
    source = ../../dotfiles/dot_gitconfig;
  };
  xdg.configFile."lf/lfrc".source = ../../dotfiles/dot_config/lf/lfrc;
  xdg.configFile."starship.toml".source = ../../dotfiles/dot_config/starship.toml;
}

