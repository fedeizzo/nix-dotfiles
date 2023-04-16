;;; fi-ui.el --- TODO

;;; Commentary:


;;; Code:
(fi/load-package-config UI-MODULE-PATH "doom-modeline.el")
(fi/load-package-config UI-MODULE-PATH "doom-themes.el")
(fi/load-package-config UI-MODULE-PATH "hl-todo.el")
(fi/load-package-config UI-MODULE-PATH "idle-highlight.el")
(fi/load-package-config UI-MODULE-PATH "ligatures.el")
(fi/load-package-config UI-MODULE-PATH "minimap.el")
(fi/load-package-config UI-MODULE-PATH "rainbow-delimiters.el")

;; (defun fi/font-config (frame)
;;   (set-face-attribute 'default nil :font "JetBrains Mono" :weight 'normal :height 130)
;;   (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'normal :height 130)
;;   (set-face-attribute 'variable-pitch nil :font "JetBrains Mono" :weight 'normal :height 130))

(defun fi/update-font--window-size-change (&rest _)
  (let* ((attrs (frame-monitor-attributes))
	 (width-mm (nth 1 (nth 2 attrs)))
	 (size 12))         ;; default size for the internal laptop monitor
    (when (eq width-mm 602) ;; office monitor
      (setq size 13))
    (set-frame-font (format "JetBrains Mono %s" size))))

;; Better welcome page https://xenodium.com/emacs-a-welcoming-experiment/
(setq initial-scratch-message nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      startup-screen-inhibit-startup-screen t)
(add-hook 'after-init-hook #'fi/update-font--window-size-change)
(add-hook 'move-frame-functions #'fi/update-font--window-size-change)
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

(provide 'fi-ui)

;;; fi-ui.el ends here
