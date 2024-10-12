;;; flycheck.el --- Flycheck configuration

;;; Commentary:
;; flycheck configuration

;;; Code:
;; some configs are copied from doomeamcs
(use-package flycheck
  :custom
  (flycheck-idle-change-delay 1.0 "Waits 1s before check")
  (flycheck-buffer-switch-check-intermediate-buffers t "Checks also buffers visited quickly")
  (flycheck-display-errors-delay 0.25 "Waits 0.25s before displaying errors")
  ;; Rerunning checks on every newline is a mote excessive.
  ;; (delq 'new-line flycheck-check-syntax-automatically)
  :config
  (global-flycheck-mode))

(use-package flycheck-posframe
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))
  ;; (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p)
  ;; (add-hook 'flycheck-posframe-inhibit-functions #'evil-replace-state-p))

(use-package flycheck-projectile
  :after flycheck)

;;; flycheck.el ends here
