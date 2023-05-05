;;; orderless.el --- TODO

;;; Commentary:
;; TODO

;;; Code:
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t))

;;; orderless.el ends here
