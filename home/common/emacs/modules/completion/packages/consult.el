;;; consult.el --- Consult configuration

;;; Commentary:
;; Consult offers several commands to do many stuff

;;; Code:
(use-package consult
  :bind
  ("C-c h" . consult-history)
  ("M-o" . consult-projectile-switch-to-buffer)
  ("C-s" . consult-line)
  ("M-y" . consult-yank-from-kill-ring)
  :config
  (consult-customize org-roam-node-find :preview-key nil)
  (consult-customize org-roam-node-insert :preview-key nil))

(defun fi/consult-ripgrep-specific-dir ()
  (interactive)

  (consult--grep "Ripgrep" #'consult--ripgrep-make-builder dir initial)
  (let ((dir
         (consult--read
          (butlast (split-string
                    (shell-command-to-string
                     (concat
                      "fd . -t d --full-path "
                      (projectile-project-root)))
                    "\n"))
          :prompt "Filter dir: "
          :sort t
          :require-match t
          :category 'file
          :history 'file-name-history)))
    (consult-ripgrep dir)
    ))

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
  (consult-org-roam-buffer-after-buffers nil))

;; eglot integratino that offers the consult-eglot-symbols command
(use-package consult-eglot
  :commands (consult-eglot-symbols))

(use-package consult-lsp
  :commands (consult-lsp-symbols consult-lsp-file-symbols)
  :after lsp)


;;; consult.el ends here
