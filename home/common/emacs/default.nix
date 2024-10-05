{ config, pkgs, libs, ... }:
let
  myEmacs = (pkgs.emacsWithPackagesFromUsePackage {
    config = ''
      (load-file "~/.config/emacs/init.el")
    '';
    defaultInitFile = true;
    package = pkgs.emacs-pgtk;
    alwaysEnsure = true;
    extraEmacsPackages = epkgs: with epkgs; [
      (import ./modules/checkers { epkgs = epkgs; }).packages
      (import ./modules/completion { epkgs = epkgs; }).packages
      (import ./modules/org { epkgs = epkgs; }).packages
      (import ./modules/ui { epkgs = epkgs; }).packages
      (import ./modules/prog { pkgs = pkgs; epkgs = epkgs; }).packages
      (import ./modules/keys { epkgs = epkgs; }).packages
      (import ./modules/life-improvements { epkgs = epkgs; }).packages
      # nano-theme
      # nano-modeline
    ];
  });
  org-cv = pkgs.fetchFromGitLab {
    owner = "fedeizzo";
    repo = "org-cv";
    rev = "master";
    sha256 = "sha256-OQ0WuMXHPusxLPpuVqkq7t1IDZx4ZvPyKdc4h+8QDAs=";
  };
  hydra-posframe = pkgs.fetchFromGitHub {
    owner = "Ladicle";
    repo = "hydra-posframe";
    rev = "master";
    sha256 = "sha256-9nVBnpaWZIYNDvS2WWBED0HsIRIv4AR4as6wEe463tI=";
  };
  org-outer-indent = pkgs.fetchFromGitHub {
    owner = "rougier";
    repo = "org-outer-indent";
    rev = "master";
    sha256 = "sha256-Lxusc3FXag4qVJjObg6EVcILFnHZXXAyrYNqqCZZF3E=";
  };
  gotest-ui-mode = pkgs.fetchFromGitHub {
    owner = "boinkor-net";
    repo = "gotest-ui-mode";
    rev = "master";
    sha256 = "sha256-220Aw6adN7tDW9f8lZQnWRPEX6Pt8YNO5Lrwb30NsWg=";
  };
  ts-fold = pkgs.fetchFromGitHub {
    owner = "emacs-tree-sitter";
    repo = "ts-fold";
    rev = "master";
    sha256 = "sha256-Ao6vOS8PdVPo+E+lxH0TWfHwUSMFcLFgaxGG/D3I+rc=";
  };
in
{
  home.packages = with pkgs; [
    myEmacs
    emacs-lsp-booster
    # poppler
    poppler_utils
    imagemagick
    # spell
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science it ]))
    enchant
  ];
  home.sessionVariables.ASPELL_CONF = "dict-dir ${pkgs.aspellWithDicts (ds: with ds; [ en en-computers en-science it ])}/lib/aspell";

  xdg.configFile."emacs/early-init.el".source = ./early-init.el;
  xdg.configFile."emacs/init.el".source = ./init.el;
  xdg.configFile."emacs/welcome.png".source = ./welcome.png;
  xdg.configFile."emacs/modules".source = ./modules;
  xdg.configFile."emacs/org-cv".source = org-cv;
  xdg.configFile."emacs/hydra-posframe".source = hydra-posframe;
  xdg.configFile."emacs/org-outer-indent".source = org-outer-indent;
  xdg.configFile."emacs/gotest-ui-mode".source = gotest-ui-mode;
  xdg.configFile."emacs/ts-fold".source = ts-fold;
}
