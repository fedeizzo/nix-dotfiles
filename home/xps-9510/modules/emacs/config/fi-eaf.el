(use-package eaf
  :load-path "~/.config/emacs/emacs-application-framework"
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (setq eaf-wm-name "wlroots wm")
  ;; (setq eaf-wm-focus-fix-wms "wlroots wm")
  (defalias 'browse-web #'eaf-open-browser))
;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wik
(setq eaf-enable-debug t)
(require 'eaf-browser)
(require 'eaf-pdf-viewer)
(require 'eaf-image-viewer)
(require 'eaf-markdown-previewer)
(require 'eaf-org-previewer)
(require 'eaf-file-manager)
(require 'eaf-jupyter)
(provide 'fi-eaf)
