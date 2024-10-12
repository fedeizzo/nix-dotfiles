{ epkgs }:

{
  packages = with epkgs; [
    avy
    elfeed
    elfeed-web
    elfeed-org
    elfeed-summary
    dash
    direnv
    deft
    eshell-z
    eshell-vterm
    eshell-syntax-highlighting
    eshell-git-prompt
    exec-path-from-shell # fix the mismatch between macos PATH and emacs PATH
    format-all
    hide-mode-line
    # origami
    pdf-tools
    popwin
    s
    sideline
    sideline-blame
    super-save
    tablist
    treemacs
    treemacs-projectile
    # treemacs-magit
    treemacs-evil
    treemacs-all-the-icons
    vundo
    zoom
    rainbow-delimiters
    ripgrep
    rg
    vterm
    esup
    ace-window
    centaur-tabs
  ];
}
