;;; embark.el --- Embark configuration

;;; Commentary:
;; Embark configuration

;;; Code:
(use-package embark
  :bind
  ("C-h B" . embark-bindings)
  ("M-e" . #'embark-act)
  :custom
  (prefix-help-command #'embark-prefix-help-command))
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; embark.el ends here
