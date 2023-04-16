;; fix the mismatch between macos PATH and emacs PATH
(use-package exec-path-from-shell
  :if ON-MACBOOK
  :hook
  (after-init . exec-path-from-shell-initialize)
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH")))
