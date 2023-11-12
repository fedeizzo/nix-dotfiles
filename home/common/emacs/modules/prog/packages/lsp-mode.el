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
  ;; python
  (python-mode . lsp-deferred)
  (python-ts-mode . lsp-deferred)
  ;; nix
  (nix-mode . lsp-deferred)
  ;; typescript, javascript, json
  (typescript-mode . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  (typescript-ts-base-mode . lsp-deferred)
  (javascript-mode . lsp-deferred)
  (javascript-ts-mode . lsp-deferred)
  (json-ts-mode . lsp-deferred)
  (json-mode . lsp-deferred)
  ;; go
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
  (lsp-ui-doc-use-webkit t)
  (lsp-completion-provider :none) ;; we use Corfu!
  (lsp-signature-render-documentation nil)
  (lsp-inlay-hint-enable t)
  :config
  (lsp-register-custom-settings
   '(("gopls.hints" ((assignVariableTypes . t)
                     (compositeLiteralFields . t)
                     (compositeLiteralTypes . t)
                     (constantValues . t)
                     (functionTypeParameters . t)
                     (parameterNames . t)
                     (rangeVariableTypes . t))))))

(use-package lsp-ui
  :custom
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-symbol t)
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

;; (fi/leader
;; "d" #'dap-hydra
;; "l" 'lsp-mode-toggle-hydra/body)
;; (use-package lsp-origami
;;   :hook
;;   (lsp-after-open . #'lsp-origami-try-enable))

;;; lsp-mode.el ends here
