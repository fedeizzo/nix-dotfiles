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
    # fast capture with org-protocol
    (makeDesktopItem {
      name = "org-protocol";
      exec = "emacsclient %u";
      comment = "Org protocol";
      desktopName = "org-protocol";
      type = "Application";
      mimeType = "x-scheme-handler/org-protocol";
    })
    # poppler
    poppler_utils
    imagemagick
    # ledger
    ledger
    unstable.hledger
  ];
  xdg.configFile."emacs/Emacs.org".source = ../dotfiles/emacs/Emacs.org;
  xdg.configFile."emacs/early-init.el".text = ''
    ;; Disable package.el in favor of straight.el
    (setq package-enable-at-startup nil)
  '';
}
