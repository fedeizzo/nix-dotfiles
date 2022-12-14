(use-package bufler
  :commands (bufler-switch-buffer bufler)
  :config
  (bufler-mode 1)
  (evil-collection-define-key 'normal 'bufler-list-mode-map
    (kbd "RET") 'bufler-list-buffer-switch
    (kbd "M-RET") 'bufler-list-buffer-peek
    "D" 'bufler-list-buffer-kill))

(defun fi/toggle-hide-help-buffer ()
  "Toggle popwin and stop automatic close of help buffer"
  (interactive)
  (if (bound-and-true-p popwin-mode)
      (popwin-mode -1)
    (popwin-mode t)))

(provide 'fi-buffer)
