(defun fi/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-super-capf
                     #'eglot-completion-at-point
                     #'tempel-expand
                     #'cape-file))))

(use-package eglot
  :config
  (setq eglot-autoreconnect t eglot-autoshutdown t)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'nix-mode-hook 'eglot-ensure)
  (add-hook 'eglot-managed-mode-hook #'fi/eglot-capf))

(pretty-hydra-define lsp-hydra-main (:color blue :title "LspMode" :quit-key "q")
  ("Buffer"
   (("f" eglot-format-buffer "format")
    ("h" eldoc "docs")
    ("c" git-gutter:popup-hunk "show hunk diff"))
   "Navigation"
   (("d" xref-find-definitions "go to definition (gd)")
    ("z" xref-find-references "go to references (gr)")
    ("s" imenu "symbols"))
   "Actions"
   (("r" eglot-rename "rename symbol")
    ("a" eglot-code-actions "all")
    ("o" eglot-code-action-organize-imports "organize imports")
    ("x" eglot-code-action-quickfix "quickfix")
    ("e" eglot-code-action-extract "extract")
    ("i" eglot-code-action-inline "inline"))))
(fi/leader "l" 'lsp-hydra-main/body)
