;;; eshell.el --- Eshell configuration

;;; Commentary:
;; Many eshell packages

;;; Code:
(defun fi/define-eshell-aliases (aliases)
  (dolist (alias aliases)
    (eshell/alias (car alias) (cdr alias))))

(use-package eshell)

(use-package eshell-z
  :after eshell)
(use-package eshell-vterm
  :after eshell
  :config
  (eshell-vterm-mode)
  (defalias 'eshell/v 'eshell-exec-visual)
  )
(use-package eshell-git-prompt
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'git-radar))

(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))

(fi/define-eshell-aliases
 '(("ls" . "eza --icons --sort=type")
   ("ll" . "eza -l --icons --sort=type")
   ("lla" . "eza -la --icons --sort=type")
   ("llt" . "eza -T --icons --sort=type")
   ("cat" . "bat")
   ("find" . "fd")
   ("clear" . "clear-scrollback")
   ("gs" . "git status")
   ("ga" . "git add")
   ("gc" . "git commit -m $1")
   ("gp". "git push")))

;;; eshell.el ends here
