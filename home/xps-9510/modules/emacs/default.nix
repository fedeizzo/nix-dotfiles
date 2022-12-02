{ config, pkgs, pkgs-unstable, libs, ... }:
let
  myEmacs = (pkgs.emacsWithPackagesFromUsePackage {
    config = "";
    defaultInitFile = true;
    package = pkgs.emacsPgtkNativeComp;
    alwaysEnsure = true;
    extraEmacsPackages = epkgs: with epkgs; [
      use-package
      # ORG
      org
      org-contrib
      org-roam
      org-roam-ui
      org-download
      org-cliplink
      org-super-agenda
      async
      ox-epub
      ox-hugo
      org-modern
      org-fragtog
      org-noter
      zetteldesk
      # org-cv
      # FACE
      ligature
      doom-themes
      doom-modeline
      all-the-icons
      visual-fill-column
      # MINIBUFFER
      amx
      ivy
      ivy-rich
      counsel
      helpful
      # COMPLETION
      company
      company-box
      # NIX
      direnv
      # BENCHMARK
      esup
      # BUFFER
      bufler
      # FOLDING
      s
      dash
      origami
      # FORMAT
      format-all
      # KEYBIND
      hydra
      major-mode-hydra
      evil
      evil-collection
      evil-commentary
      general
      # MONEY
      hledger-mode
      eglot
      nix-mode
      markdown-toc
      rustic
      yaml-mode
      # gendoxy
      # yuck-mode
      magit
      notmuch
      rainbow-delimiters
      tablist
      pdf-tools
      popwin
      projectile
      ripgrep
      swiper
      yasnippet
      yasnippet-snippets
      super-save
      flyspell-correct
      avy
      tree-sitter
      tree-sitter-langs
      vundo
      which-key
      zoom
      auctex
      deft
      vterm
    ];
  });
  org-cv = pkgs.fetchFromGitLab {
    owner = "fedeizzo";
    repo = "org-cv";
    rev = "master";
    sha256 = "sha256-OQ0WuMXHPusxLPpuVqkq7t1IDZx4ZvPyKdc4h+8QDAs=";
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
    # borg package manager
    gnumake
  ];
  xdg.configFile."emacs/init.el".source = ./init.el;
  xdg.configFile."emacs/config".source = ./config;
  xdg.configFile."emacs/org-cv".source = org-cv;
  xdg.configFile."emacs/gendoxy".source = gendoxy;
}
