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
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
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
  ;; java
  (java-mode . lsp-deferred)
  (java-ts-mode . lsp-deferred)
  ;; go
  (go-mode . lsp-deferred)
  (go-ts-mode . lsp-deferred)
  ;; dockerfile
  (dockerfile-mode . lsp-deferred)
  (dockerfile-ts-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-completion-mode . fi/lsp-mode-setup-completion)
  (buffer-list-update . fi/lsp-ui-imenu-update)
  :custom
  (lsp-auto-configure t)
  (lsp-auto-guess-root t)
  (lsp-before-save-edits t)
  (lsp-enable-on-type-formatting nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-eldoc-render-all t)
  (lsp-enable-suggest-server-download nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-use-webkit t)
  (lsp-completion-provider :none) ;; we use Corfu!
  (lsp-signature-render-documentation nil)
  (lsp-semgrep-languages (list))
  (lsp-enable-snippet nil)
  ;; (lsp-inlay-hint-enable t)
  ;; go
  (lsp-go-codelenses
   '(
     (gc_details . :json-false)
     (generate . :json-false)
     (regenerate_cgo . :json-false)
     (tidy . :json-false)
     (upgrade_dependency . :json-false)
     (test . :json-false)
     (vendor . :json-false)))
  :config
  (define-key lsp-mode-map (kbd "C-l") lsp-command-map))

(use-package lsp-ui
  :custom
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-symbol nil)
  :config
  (lsp-ui-peek-enable t))

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode))

(use-package dap-mode
  :custom
  (dap-ui-variable-length 100)
  :config
  (setq dap-auto-configure-features '(locals controls tooltip breakpoints))
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

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

(with-eval-after-load 'dap-mode
  (setq dap-debug-template-configurations nil)
  (dap-register-debug-template " Test Current Function"
                               (list :type "go"
                                     :request "launch"
                                     :name "Test function"
                                     :mode "test"
                                     :program nil
                                     :args nil
                                     :buildFlags "-tags dynamic"
                                     :env nil))
  (dap-register-debug-template " Test Current Function - integration"
                               (list :type "go"
                                     :request "launch"
                                     :name "Test function"
                                     :mode "test"
                                     :program nil
                                     :args nil
                                     :buildFlags "-tags dynamic -args integration"
                                     :env nil))
  (dap-register-debug-template " Test Current Subtest"
                               (list :type "go"
                                     :request "launch"
                                     :name "Test subtest"
                                     :mode "test"
                                     :program nil
                                     :args nil
                                     :buildFlags "-tags dynamic"
                                     :env nil))
  (dap-register-debug-template " Test Current Subtest - integration"
                               (list :type "go"
                                     :request "launch"
                                     :name "Test subtest"
                                     :mode "test"
                                     :program nil
                                     :args nil
                                     :buildFlags "-tags dynamic -args integration"
                                     :env nil)))

;; (fi/leader
;; "d" #'dap-hydra
;; "l" 'lsp-mode-toggle-hydra/body)
;; (use-package lsp-origami
;;   :hook
;;   (lsp-after-open . #'lsp-origami-try-enable))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;;; lsp-mode.el ends here
