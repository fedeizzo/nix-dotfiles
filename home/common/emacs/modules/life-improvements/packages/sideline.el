;;; sideline.el --- sideline and extensions configuration

;;; Commentary:
;;

;;; Code:

(use-package sideline
  :config
  (setq sideline-order-left 'up
        sideline-order-right 'up
        sideline-backends-right '((sideline-blame . up))
        sideline-delay 1.0
        sideline-display-backend-name t
        sideline-backends-skip-current-line t)
  (global-sideline-mode 1))

;;; sideline.el ends here
