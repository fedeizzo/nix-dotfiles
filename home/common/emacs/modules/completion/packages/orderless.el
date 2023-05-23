;;; orderless.el --- TODO

;;; Commentary:
;; TODO

;;; Code:
(use-package orderless
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t))

;; (completion-category-overrides '((file (styles basic partial-completion))
;; 				   (eglot (styles orderless))))
;;; orderless.el ends here
