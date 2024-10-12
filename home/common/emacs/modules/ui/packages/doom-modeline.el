;; modeline
(use-package doom-modeline
  :custom
  (doom-modeline-icon t)
  (doom-modeline-height 1)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-modal nil)
  (doom-modeline-time nil)
  (doom-modelin-battery nil)
  (doom-modeline-major-mode-color-icon t)
  :init (doom-modeline-mode 1))


;; icons
(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-nerd-fonts
  :load-path "~/.config/emacs/all-the-icons-nerd-fonts"
  :after all-the-icons
  :config
  (all-the-icons-nerd-fonts-prefer))
