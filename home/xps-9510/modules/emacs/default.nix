{ config, pkgs, pkgs-unstable, libs, ... }:
let
  myEmacs = with pkgs; ((emacsPackagesFor emacsPgtkNativeComp).emacsWithPackages (epkgs: [
    epkgs.vterm
    epkgs.org-roam-ui
  ]));
  my-python-packages = python-packages: with python-packages; [
    pyqt6
    sip
    qtpy
    pyqt6-webengine
    epc
    lxml # for eaf
    qrcode # eaf-file-browser
    pysocks # eaf-browser
    pymupdf # eaf-pdf-viewer
    pypinyin # eaf-file-manager
    psutil # eaf-system-monitor
    retry # eaf-markdown-previewer
    markdown
  ];
  python-with-my-packages = pkgs-unstable.python3.withPackages my-python-packages;
  org-cv = pkgs.fetchFromGitLab {
    owner = "fedeizzo";
    repo = "org-cv";
    rev = "master";
    sha256 = "sha256-OQ0WuMXHPusxLPpuVqkq7t1IDZx4ZvPyKdc4h+8QDAs=";
  };
  ligature = pkgs.fetchFromGitHub {
    owner = "mickeynp";
    repo = "ligature.el";
    rev = "master";
    sha256 = "sha256-o6iL4mwTzfD7JOlWP4Mv27+nRGplcseGjam7WIlHZTc=";
  };
  zetteldesk = pkgs.fetchFromGitHub {
    owner = "Vidianos-Giannitsis";
    repo = "zetteldesk.el";
    rev = "master";
    sha256 = "sha256-sH2AQHLIKjM5HOrs04vw4s/a+7MsL7h7S6ERvNGy508=";
  };
  gendoxy = pkgs.fetchFromGitHub {
    owner = "mp81ss";
    repo = "gendoxy";
    rev = "master";
    sha256 = "sha256-z3L5VScaQ7LssIvCXjRsKbR7yHdlaamGhTkClSE/MJo=";
  };
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
      mimeTypes = [ "x-scheme-handler/org-protocol" ];
    })
    # poppler
    poppler_utils
    imagemagick
    # ledger
    ledger
    # spell
    aspell
    aspellDicts.en
    aspellDicts.en-science
    aspellDicts.en-computers
    aspellDicts.it
    # telega
    tdlib
    # EAF
    # python-with-my-packages
    # qt6.qtwebengine
    git
    nodejs
    wmctrl
    # eaf-browser
    aria
    # eaf-file-manager
    fd
  ];
  xdg.configFile."emacs/Emacs.org".source = ./Emacs.org;
  xdg.configFile."emacs/early-init.el".text = ''
    ;; Disable package.el in favor of straight.el
    (setq package-enable-at-startup nil)
  '';
  xdg.configFile."emacs/org-cv".source = org-cv;
  xdg.configFile."emacs/ligature.el".source = ligature;
  xdg.configFile."emacs/zetteldesk.el".source = zetteldesk;
  xdg.configFile."emacs/gendoxy".source = gendoxy;
}
