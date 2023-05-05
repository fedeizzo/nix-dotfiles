;;; go-lang.el --- Go language tools configuration

;;; Commentary:
;; go mode


;;; Code:
(use-package go-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook 'eglot-ensure))

;;; go-lang.el ends here
