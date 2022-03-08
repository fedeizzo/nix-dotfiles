{ config, pkgs, libs, ... }:

{
  programs.emacs = {
    enable = true;
    package = with pkgs; ((emacsPackagesFor emacsPgtk).emacsWithPackages (epkgs: [
      epkgs.vterm
      # epkgs.telega
      epkgs.org-roam-ui
    ]));
  };
  home.packages = with pkgs; [
    # poppler
    poppler_utils
    imagemagick
  ];
  xdg.configFile."emacs/Emacs.org".source = ../dotfiles/emacs/Emacs.org;
}
