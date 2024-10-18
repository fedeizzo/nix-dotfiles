;;; cape.el --- Completion framework

;;; Commentary:
;; cape provides several completion functions
;;; Code:
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  )

;;; cape.el ends here
