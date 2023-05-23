;;; go-lang.el --- Go language tools configuration

;;; Commentary:
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;;; Code:
(use-package go-mode
  :mode "\\.go\\'"
  :hook
  (go-mode . lsp-go-install-save-hooks)
  :config
  (evil-define-key 'normal 'go-mode-map "gd" 'lsp-ui-peek-find-definitions)
  (evil-define-key 'normal 'go-mode-map "gr" 'lsp-ui-peek-find-references)
  (evil-define-key 'normal 'go-mode-map "K" 'lsp-ui-doc-glance)
  :mode-hydra
  ((list go-ts-mode go-mode)
   (:title (fi/hydra-title-factory-fileicon "go" "Commands") :quit-key "q")
   (" LSP"
    (("lf" lsp-format-buffer "Format buffer")
     ;; ("lo" lsp-organize-imports "Organize imports")
     ("gd" lsp-ui-peek-find-definitions "Definitions")
     ("gr" lsp-ui-peek-find-references "References")
     ("K" lsp-ui-doc-glance "Documentation"))
    " Test"
    (("tf" go-test-current-file "Current file")
     ("tt" go-test-current-test "Current test")
     ("tp" go-test-current-project "Current project")
     ("tg" go-gen-test-dwim "Generate test")))))

(use-package gotest
  :custom
  (go-test-args "-tags dynamic"))

(use-package go-gen-test)


;;; go-lang.el ends here
