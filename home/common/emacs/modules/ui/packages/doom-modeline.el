;; modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  (setq doom-modeline-height 15))

;; icons
(use-package all-the-icons
  :if (display-graphic-p))
