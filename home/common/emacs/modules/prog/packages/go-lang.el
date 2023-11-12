;;; go-lang.el --- Go language tools configuration

;;; Commentary:

;;; Code:
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(defun fi/treesit-install-save-hooks ()
  (add-hook 'after-save-hook #'fi/treesit-golang-hide-oneline-returning-if-statements t t))

(defun fi/go-lang-enable-integration-test ()
  "Toggle go-lang integration test."
  (interactive)
  (if (= (length gotest-ui-additional-test-args) 2)
      (setq gotest-ui-additional-test-args '("-tags" "dynamic" "-args" "integration"))
    (setq gotest-ui-additional-test-args '("-tags" "dynamic")))
  (print gotest-ui-additional-test-args))

(use-package go-mode
  :mode "\\.go\\'"
  :hook
  (go-mode . lsp-go-install-save-hooks)
  (go-ts-mode . lsp-go-install-save-hooks)
  (go-ts-mode . fi/treesit-golang-hide-oneline-returning-if-statements)
  (go-ts-mode . fi/treesit-install-save-hooks)
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
    " Util"
    (("uh" fi/toggle-hide-oneline-returning-if-statements "Hide oneline if" :toggle fi/hide-oneline-returning-if-statements) )
    " Test"
    (("ti" fi/go-lang-enable-integration-test "Integration test")
     ("tf" #'gotest-ui-current-file "Current file")
     ("tt" #'gotest-ui-current-test "Current test")
     ;; ("tp" go-test-current-project "Current project")
     ("tg" go-gen-test-dwim "Generate test"))
    " Tag"
    (("sa" go-tag-add "Add tag")
     ("sd" go-tag-remove "Remove tag")
     ("sr" go-tag-remove "Refresh tag")))))

;; (use-package gotest
;;   :custom
;;   (go-test-args "-tags dynamic"))

(use-package go-gen-test)


(defun fi/close-gotest-buffer ()
  "Close gotest-ui buffer delete also the window."
  (interactive)
  (kill-this-buffer)
  (delete-window))

(use-package gotest-ui
  :custom
  (gotest-ui-additional-test-args '("-tags" "dynamic"))
  :load-path "~/.config/emacs/gotest-ui-mode"
  :config
  (define-key gotest-ui-mode-map (kbd "q") #'fi/close-gotest-buffer))


(use-package flycheck-golangci-lint
  :ensure t
  :hook
  (go-mode . flycheck-golangci-lint-setup)
  (go-ts-mode . flycheck-golangci-lint-setup))

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
  (go-ts-mode))


;; return snippet with default values
(defun fi/treesit-method-or-function-node-p (node)
  "Return t if the NODE type is method or function declaration."
  (let ((node-type (treesit-node-type node)))
    (or
     (string= node-type "method_declaration")
     (string= node-type "function_declaration"))
    ))

(defun fi/treesit-filter-children-parameter-declarations (node)
  "Filter parameter declaration children give a parent NODE."
  (seq-filter
   (lambda (node) (string= (treesit-node-type node) "parameter_declaration"))
   (treesit-node-children node)))

(defun fi/treesit-golang-map-node-to-default-value (node)
  "Map treesit NODE to its default value based on the node type."
  (pcase (treesit-node-text node t)
    ("error" "nil")
    ("string" "\"\"")
    ("rune" "0")
    ("int" "0")
    ("float64" "0.0")
    ("bool" "false")
    ("chan" "nil")
    ((pred (string-prefix-p "*")) "nil") ; pointer
    ((pred (string-prefix-p "<-")) "nil") ; channels
    ((pred (string-prefix-p "[")) "nil") ; arrays
    ((pred (string-match " ")) nil) ; for situations with return name
    (_ (concat (treesit-node-text node t) "{}"))
    )
  )

(defun fi/treesit-golang-default-result-parameter-list (node)
  "Get return + default value for a treesit NODE parameter list."
  (concat "return " (mapconcat
                     'fi/treesit-golang-map-node-to-default-value
                     (fi/treesit-filter-children-parameter-declarations node)
                     ", "))
  )

(defun fi/treesit-golang-default-result-type-identifier (node)
  "Get return + defaul value for treesit NODE type identiefier."
  (concat "return " (fi/treesit-golang-map-node-to-default-value node)))

(defun fi/golang-return ()
  "Return golang error check if statement with default values."
  (interactive)
  (let* ((current-node (treesit-node-at (point)))
         (function-node (treesit-parent-until current-node 'fi/treesit-method-or-function-node-p))
         (result-node (treesit-node-child-by-field-name function-node "result"))
         (result-node-type (treesit-node-type result-node))
         )
    (cond ((string= result-node-type "type_identifier") (fi/treesit-golang-default-result-type-identifier result-node))

          ((string= result-node-type "parameter_list") (fi/treesit-golang-default-result-parameter-list result-node))
          )))

;; hide onelien returnin if statement
(defvar fi/hide-oneline-returning-if-statements t "Hide if statements that contain a return statement with default values.")

(defun fi/toggle-hide-oneline-returning-if-statements ()
  (interactive)
  (hs-minor-mode)
  (if fi/hide-oneline-returning-if-statements
      (setq fi/hide-oneline-returning-if-statements nil)
    (setq fi/hide-oneline-returning-if-statements t))
  (hs-show-all)
  (fi/treesit-golang-hide-oneline-returning-if-statements))

(defun fi/treesit-golang-get-if-statements ()
  (seq-map (lambda (pos) (treesit-node-on (car pos) (cdr pos)))
           (treesit-query-range 'go "(if_statement) @fold")))

(defun fi/treesit-golang-is-oneline-if-statement-returning (node)
  (treesit-node-p (treesit-search-subtree node "return_statement")))

(defun fi/treesit-golang-filter-oneline-return-if-statements (nodes)
  (seq-filter
   'fi/treesit-golang-is-oneline-if-statement-returning
   (seq-filter
    (lambda (node) (= (-
                       (line-number-at-pos (treesit-node-end node))
                       (line-number-at-pos (treesit-node-start node)))
                      2))
    nodes)))

(defun fi/treesit-golang-fold-if-statement (node)
  (save-excursion
    (goto-char (treesit-node-start (treesit-search-subtree node "block")))
    (hs-hide-block)))

(defun fi/golang-hs-overlay (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((node (treesit-node-on (overlay-start ov) (overlay-end ov)))
           (return-node (treesit-search-subtree node "return_statement")))
      (overlay-put ov 'face 'shadow)
      (overlay-put ov 'display
                   (format "↩ %s" (string-replace "return " "" (treesit-node-text return-node))))
      )))

(defun fi/treesit-golang-hide-oneline-returning-if-statements ()
  (interactive)
  (when fi/hide-oneline-returning-if-statements
    (hs-minor-mode)
    (setq-local hs-set-up-overlay 'fi/golang-hs-overlay)
    (dolist (node
             (fi/treesit-golang-filter-oneline-return-if-statements
              (fi/treesit-golang-get-if-statements)))
      (fi/treesit-golang-fold-if-statement node))))



;;; go-lang.el ends here
