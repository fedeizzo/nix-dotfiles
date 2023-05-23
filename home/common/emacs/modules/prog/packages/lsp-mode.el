;;; lsp-mode.el --- LSP mode configuration

;;; Commentary:
;;

;;; Code:

(defun fi/lsp-ui-imenu-update ()
  (when lsp-mode
    (lsp-ui-imenu--refresh)))

(defun fi/lsp-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(flex))) ;; Configure flex
(use-package lsp-mode
  :commands lsp
  :hook
  (python-mode . lsp-deferred)
  (python-ts-mode . lsp-deferred)
  (nix-mode . lsp-deferred)
  (typescript-mode . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  (typescript-ts-base-mode . lsp-deferred)
  (javascript-mode . lsp-deferred)
  (go-mode . lsp-deferred)
  (go-ts-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-completion-mode . fi/lsp-mode-setup-completion)
  (buffer-list-update . fi/lsp-ui-imenu-update)
  :custom
  (lsp-keymap-prefix "C-l")
  (lsp-auto-configure t)
  (lsp-auto-guess-root t)
  (lsp-before-save-edits t)
  (lsp-enable-on-type-formatting nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-eldoc-render-all t)
  (lsp-enable-suggest-server-download nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-position 'at-point)
  (lsp-completion-provider :none) ;; we use Corfu!
  )

(use-package lsp-ui
  :config
  (lsp-ui-peek-enable t))

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode))

(use-package dap-mode)

(use-package dap-python
  :custom
  (dap-python-debugger 'debugpy))

(use-package dap-dlv-go)

(pretty-hydra-define lsp-mode-toggle-hydra
  (:title (fi/hydra-title-factory-faicon "toggle-on" "LSP Toggles") :quit-key "q")
  ("Modeline"
   (("md" lsp-modeline-diagnostics-mode "Diagnostic" :toggle t)
    ("ma" lsp-modeline-code-actions-mode "Code actions" :toggle t))
   "UI"
   (("ud" lsp-ui-doc-mode "Popup documentation" :toggle t)
    ("uh" lsp-toggle-symbol-highlight "Symbol highlight" :toggle t)
    ("ul" lsp-lens-mode "Lens" :toggle t)
    )
   )
  )

(fi/leader
  "d" #'dap-hydra
  "l" 'lsp-mode-toggle-hydra/body)
;; (use-package lsp-origami
;;   :hook
;;   (lsp-after-open . #'lsp-origami-try-enable))

;;; lsp-mode.el ends here
