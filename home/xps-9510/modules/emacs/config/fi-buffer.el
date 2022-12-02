(use-package bufler
  :commands (bufler-switch-buffer bufler)
  :config
  (bufler-mode 1)
  (evil-collection-define-key 'normal 'bufler-list-mode-map
    (kbd "RET") 'bufler-list-buffer-switch
    (kbd "M-RET") 'bufler-list-buffer-peek
    "D" 'bufler-list-buffer-kill))

(provide 'fi-buffer)
