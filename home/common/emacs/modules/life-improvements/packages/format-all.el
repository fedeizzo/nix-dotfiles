;;; format-all.el --- format-all configuration

;;; Commentary:
;; 

;;; Code:
(use-package format-all
  :commands (format-all-buffer format-all-region)
  :custom
  (format-all-show-errors 'never)
  :hook
  (prog-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter))

;;; format-all.el ends here
