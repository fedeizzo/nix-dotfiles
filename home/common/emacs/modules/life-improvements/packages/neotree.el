(use-package neotree
  :commands (neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--with-buffer
             neo-global--window-exists-p)
  :hook
  (neo-after-create . (lambda (a) (display-line-numbers-mode -1)))
  :config
  (setq
   neo-window-width 30
   neo-show-hidden-files t
   neo-theme 'nerd
   neo-smart-open t
   projectile-switch-project-action 'neotree-projectile-action
   neo-hidden-regexp-list
   '(;; vcs folders
     "^\\.\\(?:git\\|hg\\|svn\\)$"
     ;; compiled files
     "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
     ;; generated files, caches or local pkgs
     "^\\(?:node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
     ;; org-mode folders
     "^\\.\\(?:sync\\|export\\|attach\\)$"
     ;; temp files
     ".*~$"
     "^#.*#$")))
