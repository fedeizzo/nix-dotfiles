;;; sidecar-locals.el --- Sidecar-locals configuration

;;; Commentary:
;; Per project configuration

;;; Code:
(use-package sidecar-locals
  :custom
  (sidecar-locals-paths-allow (list
			       (concat (file-symlink-p (getenv "DATADOG_ROOT")) "/")
			       (concat (getenv "DATADOG_ROOT") "/")))
  :config
  (sidecar-locals-mode))

;;; sidecar-locals.el ends here
