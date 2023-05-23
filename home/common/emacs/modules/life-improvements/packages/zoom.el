(defun fi/fix-lsp-ui-imenu-size ()
  (with-selected-window (get-buffer-window "*lsp-ui-imenu*")
    (setq window-size-fixed t)
    (window-resize (selected-window) (- 30 (window-total-width)) t t)))

(use-package zoom
  :custom
  (zoom-ignored-buffer-names '("*lsp-ui-imenu*"))
  (zoom-ignored-major-modes '("lsp-ui-imenu"))
  :config
  (zoom-mode 1))

;; (remove-hook 'lsp-ui-imenu-buffer-mode-hook #'fi/fix-lsp-ui-imenu-size)
