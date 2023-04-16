;;; consult.el --- Consult configuration

;;; Commentary:
;; Consult offers several commands to do many stuff

;;; Code:
(use-package consult
  :bind
  ("C-c h" . consult-history)
  ("M-o" . consult-buffer)
  ("C-s" . consult-line)
  :config
  (consult-customize org-roam-node-find :preview-key nil)
  (consult-customize org-roam-node-insert :preview-key nil))

(defun wrapper/consult-ripgrep (&optional dir given-initial)
  "Pass the region to consult-ripgrep if available.

DIR and GIVEN-INITIAL match the method signature of `consult-wrapper'."
  (interactive "P")
  (let ((initial
         (or given-initial
             (when (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))))))
    (consult-ripgrep dir initial)))

(use-package consult-org-roam
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t))

;; eglot integratino that offers the consult-eglot-symbols command
(use-package consult-eglot
  :commands (consult-eglot-symbols))

;;; consult.el ends here
