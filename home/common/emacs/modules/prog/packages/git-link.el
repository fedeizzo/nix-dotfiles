;;; git-link.el --- open file in GitHub

;;; Commentary:
;;

;;; Code:

(use-package git-link
  :bind (("C-c g l" . 'git-link))
  :custom
  (git-link-open-in-browser t))

;;; git-link.el ends here
