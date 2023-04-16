;;; cape.el --- Cape configuration

;;; Commentary:
;; cape provides several completion functions

;;; Code:
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

;;; cape.el ends here
