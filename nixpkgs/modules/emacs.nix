{ config, pkgs, libs, ... }:
let
  myEmacs = with pkgs; ((emacsPackagesFor emacsPgtkNativeComp).emacsWithPackages (epkgs: [
    epkgs.vterm
    epkgs.org-roam-ui
  ]));
in
{
  programs.emacs = {
    enable = true;
    package = myEmacs;
  };
  services.emacs = {
    enable = true;
    package = myEmacs;
    client = {
      enable = true;
    };
  };
  home.packages = with pkgs; [
    # poppler
    poppler_utils
    imagemagick
    # ledger
    ledger
    unstable.hledger
  ];
  xdg.configFile."emacs/Emacs.org".source = ../dotfiles/emacs/Emacs.org;
}
