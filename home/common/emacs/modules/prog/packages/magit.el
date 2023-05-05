(use-package magit
  :custom
  (magit-refresh-status-buffer nil "Performance optimization")
  :commands magit-status)

(use-package magit-todos
  :config
  (magit-todos-mode t))
