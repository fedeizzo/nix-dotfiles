(use-package format-all
  :commands (format-all-buffer format-all-region)
  :hook
  (prog-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter))
