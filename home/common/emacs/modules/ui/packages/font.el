;;; font.el --- Font configuration

;;; Commentary:
;; Personal font configuration using CaskaydiaCove Nerd Font

;;; Code:
(set-face-attribute 'default nil
		    :family "CaskaydiaCove Nerd Font"
		    :weight 'light)
(set-face-attribute 'bold nil
		    :family "CaskaydiaCove Nerd Font"
		    :weight 'regular)
(set-face-attribute 'italic nil
		    :family "CaskaydiaCove Nerd Font"
		    :weight 'semilight
		    :slant 'italic)

(if ON-MACBOOK
    (defun fi/update-font--window-size-change (&rest _)
      "Update font size based on the screen resolution."
      (let* ((attrs (frame-monitor-attributes))
	     (width-mm (nth 1 (nth 2 attrs)))
	     (size 12))         ;; default size for the internal laptop monitor
        (when (or (eq width-mm 602) (eq width-mm 801))  ;; office and home monitor
          (setq size 13))
        (set-frame-font (format "CaskaydiaCove Nerd Font %s" size))))
  (defun fi/update-font--window-size-change (&rest _)
    "Update font size based on the screen resolution."
    (set-frame-font (format "CaskaydiaCove Nerd Font %s" 11))
    ))

(defun fi/update-font-size ()
  "Update font size based on the screen resolution."
  (interactive)
  (fi/update-font--window-size-change))

(add-hook 'after-init-hook #'fi/update-font--window-size-change)
(add-hook 'move-frame-functions #'fi/update-font--window-size-change)

;;; font.el ends here
