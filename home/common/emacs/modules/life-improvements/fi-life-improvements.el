;;; Variables
;; (setq vterm-shell "fish")
(setq vterm-kill-buffer-on-exit t)

;;; Function

;;; Import
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "avy.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "dash.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "deft.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "direnv.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "elfeed.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "eshell.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "sideline.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "exec-path-from-shell.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "format-all.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "hide-mode-line.el")
;; (fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "neotree.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "treemacs.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "origami.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "pdf-tools.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "popwin.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "s.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "super-save.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "vundo.el")
(fi/load-package-config LIFE-IMPROVEMENTS-MODULE-PATH "zoom.el")

;;; Modes
(electric-pair-mode 1)

(provide 'fi-life-improvements)
