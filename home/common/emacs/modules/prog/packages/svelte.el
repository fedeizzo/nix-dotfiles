;;; svelte.el --- svelte configuration

;;; Commentary:
;;

;;; Code:


(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
  (setq web-mode-engines-alist
        '(("svelte" . "\\.svelte\\'"))))

;;; svelte.el ends here
