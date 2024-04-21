;;; projectile.el ---

;;; Commentary:
;;

;;; Code:
(use-package projectile
  :diminish projectile-mode
  :custom
  (projectile-enable-caching t)
  (projectile-switch-project-action #'project-dired)
  (projectile-indexing-method 'alien)
  (projectile-sort-order 'recently-active)
  (projectile-auto-discover nil)
  :config (projectile-mode)
  :init
  (if ON-MACBOOK
      (setq projectile-project-search-path '("~/dd/" "~/uni" "~/personal"))
    (setq projectile-project-search-path '("~/personalProject" "~/uni"))))

(defun fi/consult-projectile-wrap ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (if project-root
        (consult-projectile)
      (consult-projectile '(consult-projectile--source-projectile-project)))
    ))

(pretty-hydra-define projectile-hydra-main (:color blue :title "Projectile" :quit-key "q")
  ("Global"
   (("p" fi/consult-projectile-wrap "consult projectile"))
   "Current"
   (("t" projectile-run-vterm "open terminal")
    ("T" projectile-run-vterm-other-window "open terminal")
    ("c" #'fi/get-project-filepath "copy relative current buffer")
    ("k" projectile-kill-buffers "close project"))
   "Search"
   (("g" consult-ripgrep "ripgrep")
    ("G" #'fi/consult-ripgrep-specific-dir "dir + ripgrep")
    ("s" consult-lsp-symbols "lsp symbol")))
  )

;;; projectile.el ends here
