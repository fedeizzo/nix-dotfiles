{ config, pkgs, libs, ... }:

{
  programs.emacs = {
    enable = true;
    package = with pkgs; ((emacsPackagesFor emacsPgtkGcc).emacsWithPackages (epkgs: [ epkgs.vterm ]));
  };
  home.packages = with pkgs; [
    # poppler
    poppler_utils
    imagemagick
  ];
  xdg.configFile."emacs/Emacs.org".source = ../dotfiles/emacs/Emacs.org;
}
