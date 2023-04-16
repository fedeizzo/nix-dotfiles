(use-package popwin
  :config
  (popwin-mode 1))


(defun fi/toggle-hide-help-buffer ()
  "Toggle popwin and stop automatic close of help buffer"
  (interactive)
  (if (bound-and-true-p popwin-mode)
      (popwin-mode -1)
    (popwin-mode t)))
