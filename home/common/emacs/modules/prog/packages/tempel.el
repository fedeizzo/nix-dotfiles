;; Setup completion at point
(defun tempel-setup-capf ()
  ;; Add the Tempel Capf to `completion-at-point-functions'.
  ;; `tempel-expand' only triggers on exact matches. Alternatively use
  ;; `tempel-complete' if you want to see all matches, but then you
  ;; should also configure `tempel-trigger-prefix', such that Tempel
  ;; does not trigger too often when you don't expect it. NOTE: We add
  ;; `tempel-expand' *before* the main programming mode Capf, such
  ;; that it will be tried first.
  (setq-local completion-at-point-functions
              (cons #'tempel-complete
                    completion-at-point-functions)))
(use-package tempel
  :after cape
  :custom
  (tempel-path (expand-file-name "~/nix-dotfiles/home/common/emacs/snippets/*.eld"))
  (tempel-trigger-prefix "<") ;; this prefix is used to avoid no suggestions from lsp
  (tempel-auto-reload t)    ;; do not reload personal templates since, I have to create a new generation to update the directory
  :hook
  (prog-mode . tempel-setup-capf)
  (text-mode . tempel-setup-capf))


(use-package tempel-collection)
