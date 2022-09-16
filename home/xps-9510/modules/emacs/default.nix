{ config, pkgs, libs, ... }:
let
  myEmacs = with pkgs; ((emacsPackagesFor emacsPgtkNativeComp).emacsWithPackages (epkgs: [
    epkgs.vterm
    epkgs.org-roam-ui
  ]));
  my-python-packages = python3Packages: with python3Packages; [
    # for eaf
    pyqt5
    sip
    pyqtwebengine
    epc
    lxml
    # eaf-file-browser
    qrcode
    # eaf-browser
    pysocks
    # eaf-pdf-viewer
    pymupdf
    # eaf-file-manager
    pypinyin
    # eaf-system-monitor
    psutil
    # eaf-markdown-previewer
    retry
    markdown
  ];
  python-with-my-packages = pkgs.python39.withPackages my-python-packages;
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
    # emacs-application-framework
    nodejs
    aria
    python-with-my-packages
  ];
  xdg.configFile."emacs/Emacs.org".source = ./Emacs.org;
  xdg.configFile."emacs/early-init.el".text = ''
    ;; Disable package.el in favor of straight.el
    (setq package-enable-at-startup nil)
  '';
}
