(use-package eaf
  :load-path "~/.config/emacs/emacs-application-framework"
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (defalias 'browse-web #'eaf-open-browser))
;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wik
(require 'eaf-browser)
(setq eaf-enable-debug t)
(provide 'fi-eaf)
