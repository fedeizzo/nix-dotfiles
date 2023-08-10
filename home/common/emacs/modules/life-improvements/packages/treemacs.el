;;; treemacs.el --- Treemacs configuration

;;; Commentary:
;; treemacs configuration

;;; Code:
(use-package treemacs
  :custom
  (treemacs-width 35)
  (treemacs-wide-toggle-width 70)
  (treemacs-show-hidden-files nil)
  (treemacs-project-follow-cleanup t)
  :config
  (treemacs-follow-mode)
  (treemacs-git-mode 'deferred)
  (treemacs-filewatch-mode)
  (treemacs-resize-icons 12))

(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-evil
  :after (treemacs evil))

;; (use-package treemacs-projectile
;;   :after (treemacs projectile))

(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :custom
  (lsp-treemacs-theme "Iconless")
  :config
  (lsp-treemacs-sync-mode 1))

;;; treemacs.el ends here
