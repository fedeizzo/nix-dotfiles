;; (use-package idle-highlight-mode
;;   :hook (prog-mode . 'idle-highlight-in-visible-buffers-mode)
;;   :config
;;   :config (setq idle-highlight-idle-time 0.2)

;;   (set-face-attribute 'idle-highlight-in-visible-buffers nil :background "selectedTextBackgroundColor" :weight 'bold))
(use-package idle-highlight-mode
  :hook ((prog-mode) . idle-highlight-mode)
  :config
  (setq
   idle-highlight-visible-buffers t ; show the highlight in every buffer
   idle-highlight-idle-time 0.35)
  (idle-highlight-global-mode))
