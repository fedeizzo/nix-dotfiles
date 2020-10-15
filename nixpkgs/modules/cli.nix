{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    coreutils
    dragon-drop
    exa
    fd
    fzf
    gawk
    gnused
    gnutls
    gotop
    htop
    lf
    nix-zsh-completions
    nmap
    powertop
    starship
    universal-ctags
    unzip
    xsv
    zsh
    nixpkgs-fmt
    nixpkgs-lint
    cachix
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
