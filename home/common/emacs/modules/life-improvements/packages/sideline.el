;;; sideline.el --- sideline and extenions configuration

;;; Commentary:
;;

;;; Code:

(use-package sideline
  :config
  (setq sideline-order-left 'up
        sideline-order-right 'up
        sideline-backends-left '((sideline-blame . up))
        sideline-delay 0.2
        sideline-display-backend-name t
        sideline-backends-skip-current-line t)
  (global-sideline-mode 1))

;;; sideline.el ends here
