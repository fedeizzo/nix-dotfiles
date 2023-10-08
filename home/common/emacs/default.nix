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
      (import ./modules/checkers { epkgs = epkgs; }).packages
      (import ./modules/completion { epkgs = epkgs; }).packages
      (import ./modules/org { epkgs = epkgs; }).packages
      (import ./modules/ui { epkgs = epkgs; }).packages
      (import ./modules/prog { pkgs = pkgs; epkgs = epkgs; }).packages
      (import ./modules/keys { epkgs = epkgs; }).packages
      (import ./modules/life-improvements { epkgs = epkgs; }).packages
      # use-package # TODO I think that use-package can be removed because since it should be inside vanilla emacs

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