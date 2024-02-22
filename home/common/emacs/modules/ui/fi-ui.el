;;; fi-ui.el --- TODO

;;; Commentary:


;;; Code:
(fi/load-package-config UI-MODULE-PATH "doom-modeline.el")
(fi/load-package-config UI-MODULE-PATH "doom-themes.el")
(fi/load-package-config UI-MODULE-PATH "font.el")
(fi/load-package-config UI-MODULE-PATH "hl-todo.el")
(fi/load-package-config UI-MODULE-PATH "idle-highlight.el")
(fi/load-package-config UI-MODULE-PATH "ligatures.el")
(fi/load-package-config UI-MODULE-PATH "minimap.el")
;; (fi/load-package-config UI-MODULE-PATH "nano.el")
(fi/load-package-config UI-MODULE-PATH "rainbow-delimiters.el")
(fi/load-package-config UI-MODULE-PATH "svg-tag-mode.el")
(fi/load-package-config UI-MODULE-PATH "window-divider.el")

;; (defun fi/font-config (frame)
;;   (set-face-attribute 'default nil :font "JetBrains Mono" :weight 'normal :height 130)
;;   (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'normal :height 130)
;;   (set-face-attribute 'variable-pitch nil :font "JetBrains Mono" :weight 'normal :height 130))


;; some ui removal
(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; frame personalization
(setq default-frame-alist '((min-height . 1)  '(height . 45)
                            (min-width  . 1)  '(width  . 81)
                            (vertical-scroll-bars . nil)
                            (left-fringe . 0)
                            (right-fringe . 0)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 1)))

;; Default frame settings
(setq initial-frame-alist default-frame-alist)

;; Better welcome page https://xenodium.com/emacs-a-welcoming-experiment/
(setq-default
 inhibit-startup-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message ""
 inhibit-splash-screen t
 startup-screen-inhibit-startup-screen t)

(defun ar/show-welcome-buffer ()
  "Show *Welcome* buffer."
  (with-current-buffer (get-buffer-create "*Welcome*")
    (setq truncate-lines t)
    (let* ((buffer-read-only)
           (image-path "~/.config/emacs/welcome.png")
           (image (create-image image-path))
           (size (image-size image))
           (height (cdr size))
           (width (car size))
           (top-margin (floor (/ (- (window-height) height) 2)))
           (left-margin (floor (/ (- (window-width) width) 2)))
           (prompt-title "Welcome to Emacs!"))
      (erase-buffer)
      (setq mode-line-format nil)
      (goto-char (point-min))
      (insert (make-string top-margin ?\n ))
      (insert (make-string left-margin ?\ ))
      (insert-image image)
      (insert "\n\n\n")
      (insert (make-string (floor (/ (- (window-width) (string-width prompt-title)) 2)) ?\ ))
      (insert prompt-title))
    (setq cursor-type nil)
    (read-only-mode +1)
    (switch-to-buffer (current-buffer))
    (evil-local-set-key 'normal (kbd "q") 'kill-this-buffer)))

;; (when (< (length command-line-args) 2)
;;   (add-hook 'emacs-startup-hook (lambda ()
;;                                   (when (display-graphic-p)
;;                                     (ar/show-welcome-buffer)))))

;; smoother scrolling
(setq-default scroll-conservatively 101       ; Avoid recentering when scrolling far
              scroll-margin 2                 ; Add a margin when scrolling vertically
              recenter-positions '(5 bottom)) ; Set re-centering positions

;; Highlight current line
;; (require 'hl-line)
;; (global-hl-line-mode)
;; symbol end of line
;; Enable visual line fringe and empty line indicator
(setq visual-line-fringe-indicators
      '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil
              indicate-empty-lines nil
              indent-tabs-mode nil)
(fringe-mode)

(pixel-scroll-precision-mode)

(provide 'fi-ui)

;;; fi-ui.el ends here
