(use-package deft
  :commands deft
  :hook
  (deft-mode . (lambda () (display-line-numbers-mode 0)))
  (deft-mode . (lambda () (turn-off-evil-mode nil)))
  :config
  (setq deft-directory "~/nix-dotfiles/home/common/modules/emacs/"
	deft-extensions '("el")
	deft-use-filename-as-title t
	deft-recursive t))
