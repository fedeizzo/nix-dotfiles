{ config
, pkgs
, pkgs-unstable
, libs
, kindletoorg
, ...
}:
let
  myEmacs = (pkgs.emacsWithPackagesFromUsePackage {
    config = ''
      (load-file "~/.config/emacs/init.el")
    '';
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
      org-ref
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
      # eglot
      nix-mode
      markdown-toc
      rustic
      yaml-mode
      # gendoxy
      # yuck-mode
      magit
      pkgs.mu
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
      deft
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
    aspellDicts.fr
    # telega
    tdlib
    # borg package manager
    gnumake
    kindletoorg.packages.x86_64-linux.default
    # EAF
    nodejs
    wmctrl
    aria
    qt6.qtwebengine
    qt6.qtwayland
    jq
    gdb
  ];
  services.emanote = {
    enable = false;
    notes = [ "${config.home.homeDirectory}/zettelkasten" ];
    extraConfig = {
      path = {
        headHtml = ''|
          <script>
            window.MathJax = {
              startup: {
                ready: () => {
                  MathJax.startup.defaultReady();
                }
              },
              tex: {
                inlineMath: [['$', '$'], ['\\(', '\\)']]
              }
            };
          </script>
          <script async="" id="MathJax-script" src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>
        '';
      };
    };
  };
  xdg.configFile."emacs/init.el".source = ./init.el;
  xdg.configFile."emacs/config".source = ./config;
  xdg.configFile."emacs/org-cv".source = org-cv;
  xdg.configFile."emacs/gendoxy".source = gendoxy;
}
