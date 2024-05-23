;;; sidecar-locals.el --- Sidecar-locals configuration

;;; Commentary:
;; Per project configuration

;;; Code:
(if ON-MACBOOK
    (use-package sidecar-locals
      :config
      (setq sidecar-locals-paths-allow (list
			                (concat (file-symlink-p (getenv "DATADOG_ROOT")) "/")
			                (concat (getenv "DATADOG_ROOT") "/")))
      (sidecar-locals-mode)))

;;; sidecar-locals.el ends here
