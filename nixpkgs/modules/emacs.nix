{ config, pkgs, libs, ... }:

{
  programs.emacs = {
    package = pkgs.emacsPgtk;
    enable = true;
  };
  home.packages = with pkgs; [
    # poppler
    poppler_utils
    imagemagick
  ];
  xdg.configFile."emacs/Emacs.org".source = ../dotfiles/emacs/Emacs.org;
}
