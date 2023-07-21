{ config
, pkgs
, pkgs-unstable
, libs
, ...
}:
let
  myEmacs = (pkgs-unstable.emacsWithPackagesFromUsePackage {
    config = ''
      (load-file "~/.config/emacs/init.el")
    '';
    defaultInitFile = true;
    package = pkgs.emacsPgtk;
    alwaysEnsure = true;
    extraEmacsPackages = epkgs: with epkgs; [
      use-package # TODO I think that use-package can be removed because since it should be inside vanilla emacs
      # fix the mismatch between macos PATH and emacs PATH
      exec-path-from-shell
      ############
      # CHECKERS #
      ############
      flycheck # flymake substitute
      flycheck-projectile # flycheck over the current project
      flycheck-posframe # show messages just below the cursor
      ##############
      # COMPLETION #
      ##############
      cape # provides some completion function like file autocomplete
      consult # search and navigation commands based on the emacs completion function
      consult-projectile # provides a consult command that integrates very well with projectile
      consult-org-roam # provides a consult command for org roam
      # consult-eglot # provides a consult command for eglot
      corfu # completion framework that works on top of default completion functions
      embark-consult # embark applies a command based on the nearest element
      kind-icon # icon for corfu
      marginalia # marginal informations in the minibuffer
      orderless # brain behind filtering when searching for something 
      vertico # minibuffer alternative to default
      vertico-posframe # floating vertico in the center of the screen
      #######
      # ORG #
      #######
      org
      org-contrib
      org-roam
      org-roam-ui
      org-download
      org-cliplink
      org-super-agenda
      org-ref
      org-remark
      async
      ox-epub
      ox-hugo
      org-modern
      org-fragtog
      org-noter
      zetteldesk
      elfeed
      elfeed-web
      elfeed-org
      elfeed-summary
      # org-cv
      # FACE
      ligature
      doom-themes
      doom-modeline
      all-the-icons
      visual-fill-column
      minimap
      neotree
      hl-todo # highlight todo and other common keyword inside the buffer
      idle-highlight-mode # highlight the word under cursor after some idle time
      # MINIBUFFER
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
      evil-owl
      evil-quickscope
      general
      # MONEY
      hledger-mode
      # eglot
      nix-mode
      ##########
      # GOLANG #
      ##########
      go-mode
      gotest # run tests inside emacs
      go-gen-test # generates unit tests with gotests
      pkgs.gotests # generate unit tests
      ##########
      # PYTHON #
      ##########
      python-pytest
      markdown-toc
      rustic
      yaml-mode
      # gendoxy
      # yuck-mode
      magit
      magit-delta
      # magit-todos
      git-gutter-fringe
      pkgs.mu
      rainbow-delimiters
      tablist
      pdf-tools
      popwin
      projectile
      ripgrep
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
      tempel
      tempel-collection
      hide-mode-line
      svg-lib
      svg-tag-mode
      # nano-theme
      # nano-modeline
      sidecar-locals # per project variables
      typescript-mode

      ########
      #SHELL#
      ########
      eshell-z
      eshell-vterm
      eshell-syntax-highlighting
      eshell-git-prompt
      ############
      # LSP-MODE #
      ############
      lsp-mode
      lsp-ui # ui tools
      lsp-treemacs # integration with treesitter
      lsp-origami # integration with origami for folding
      consult-lsp # integration with consult
      dap-mode # debugger
      ############
      # SIDELINE #
      ############
      sideline
      sideline-blame
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
  org-fc = pkgs.fetchFromGitHub {
    owner = "l3kn";
    repo = "org-fc";
    rev = "master";
    sha256 = "sha256-X01yELYog1bRJb1jAk77jbjDBvJxMVLoDsw+7S4lLec=";
  };
  hydra-posframe = pkgs.fetchFromGitHub {
    owner = "Ladicle";
    repo = "hydra-posframe";
    rev = "master";
    sha256 = "sha256-7/HwWJfSEf3dkIM9yYzL5i9gp60es7GLb0WbIns5Ut8=";
  };
  org-outer-indent = pkgs.fetchFromGitHub {
    owner = "rougier";
    repo = "org-outer-indent";
    rev = "master";
    sha256 = "sha256-Lxusc3FXag4qVJjObg6EVcILFnHZXXAyrYNqqCZZF3E=";
  };
in
{
  programs.emacs = {
    enable = true;
    package = myEmacs;
  };
  home.packages = with pkgs; [
    # poppler
    poppler_utils
    imagemagick
    # spell
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science it ]))
  ];
  xdg.configFile."emacs/init.el".source = ./init.el;
  xdg.configFile."emacs/welcome.png".source = ./welcome.png;
  xdg.configFile."emacs/modules".source = ./modules;
  xdg.configFile."emacs/org-cv".source = org-cv;
  xdg.configFile."emacs/org-fc".source = org-fc;
  xdg.configFile."emacs/gendoxy".source = gendoxy;
  xdg.configFile."emacs/hydra-posframe".source = hydra-posframe;
  xdg.configFile."emacs/org-outer-indent".source = org-outer-indent;
}
