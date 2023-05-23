(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (if ON-MACBOOK
      (setq projectile-project-search-path '("~/dd/" "~/uni"))
    (setq projectile-project-search-path '("~/personalProject" "~/uni")))
  (setq
   projectile-switch-project-action #'project-dired
   projectile-indexing-method 'hybrid
   projectile-sort-order 'recently-active))
(pretty-hydra-define projectile-hydra-main (:color blue :title "Projectile" :quit-key "q")
  ("Global"
   (("p" consult-projectile "consult projectile"))
   "Current"
   (("g" consult-ripgrep "search all")
    ("t" projectile-run-eshell "open terminal")
    ("c" #'fi/get-project-filepath "copy relative current buffer")
    ("k" projectile-kill-buffers "close project")))
  )
(fi/leader "p" 'projectile-hydra-main/body)
