;;; go-lang.el --- Go language tools configuration

;;; Commentary:

;;; Code:
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(defun fi/go-lang-enable-integration-test ()
  "Toggle go-lang integration test."
  (interactive)
  (if (string= go-test-args "-tags dynamic")
      (setq go-test-args "-tags dynamic -args integration")
    (setq go-test-args "-tags dynamic"))
  (print go-test-args))

(use-package go-mode
  :mode "\\.go\\'"
  :hook
  (go-mode . lsp-go-install-save-hooks)
  (before-save-hook . gofmt-before-save)
  :config
                                        ; (evil-define-key 'normal 'go-mode-map "gd" 'lsp-ui-peek-find-definitions)
                                        ; (evil-define-key 'normal 'go-mode-map "gr" 'lsp-ui-peek-find-references)
                                        ; (evil-define-key 'normal 'go-mode-map "K" 'lsp-ui-doc-glance)
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
    (("ti" fi/go-lang-enable-integration-test "Integration test")
     ("tf" go-test-current-file "Current file")
     ("tt" go-test-current-test "Current test")
     ("tp" go-test-current-project "Current project")
     ("tg" go-gen-test-dwim "Generate test"))
    " Tag"
    (("sa" go-tag-add "Add tag")
     ("sd" go-tag-remove "Remove tag")
     ("sr" go-tag-remove "Refresh tag")))))

(use-package gotest
  :custom
  (go-test-args "-tags dynamic"))

(use-package go-gen-test)

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

(defun fi/golang-temp-main-file ()
  "Create a main file in the current directory."
  (interactive)
  (create-file-buffer "main.go")
  (switch-to-buffer "main.go")
  (insert "
package main

import (
	\"fmt\"
)

func main() {
	fmt.Println(\"hello world\")
}
")
  (go-mode))


;;; go-lang.el ends here
