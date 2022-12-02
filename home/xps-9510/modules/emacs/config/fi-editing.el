(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-dabbrev-minimum-length 4)
  (setq company-files-exclusions '(".git/" ".DS_Store"))
  (setq company-idle-delay 0.1)
  (setq company-backends '((company-yasnippet company-capf company-files))))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package direnv
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))

(use-package s)
(use-package dash)
(use-package origami
  :config
  (origami-mode))

(use-package format-all
  :commands (format-all-buffer format-all-region)
  :hook
  (prog-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter))

(use-package eglot
  :config
  (setq eglot-autoreconnect t eglot-autoshutdown t)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'nix-mode-hook 'eglot-ensure))

(pretty-hydra-define lsp-hydra-main (:color blue :title "LspMode" :quit-key "q")
  ("Project"
   (("r" eglot-rename "rename symbol")
    ("d" flymake-show-project-diagnostics "diagnostic"))
   "Buffer"
   (("f" eglot-format-buffer "format")
    ("h" eldoc "docs"))
   "Actions"
   (("a" eglot-code-actions "all")
    ("o" eglot-code-action-organize-imports "organize imports")
    ("x" eglot-code-action-quickfix "quickfix")
    ("e" eglot-code-action-extract "extract")
    ("i" eglot-code-action-inline "inline")
    ("r" eglot-code-action-rewrite "rewrite"))))
(fi/leader "l" 'lsp-hydra-main/body)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package rustic
  :mode "\\.rs\\'"
  :config
  (setq rustic-lsp-client 'eglot))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(use-package gendoxy
  :commands (gendoxy-header gendoxy-tag)
  :load-path "~/.config/emacs/gendoxy")

					; (use-package yuck-mode)
(use-package magit)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(electric-pair-mode 1)
(use-package tablist)
(use-package pdf-tools)

(use-package popwin
  :config
  (popwin-mode 1))
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom (projectile-completion-system 'ivy)
  :init
  (setq projectile-project-search-path '("~/fbk" "~/personalProject" "~/uni")
        projectile-switch-project-action #'project-dired
        projectile-indexing-method 'hybrid
        projectile-sort-order 'recently-active
        projectile-completion-system 'ivy)
  )
(pretty-hydra-define projectile-hydra-main (:color blue :title "Projectile" :quit-key "q")
  ("Global"
   (("p" projectile-switch-project "switch project"))
   "Current"
   (("f" projectile-find-file "find file")
    ("g" fi/ripgrep-regexp "search all")
    ("t" projectile-run-vterm "open terminal")
    ("k" projectile-kill-buffers "close project")))
  )
(fi/leader "p" 'projectile-hydra-main/body)

(defun fi/ripgrep-regexp (regex)
  "Custom ripgrep-regexp that adds directory with projectile"
  (interactive "sRipgrep search for: ")
  (ripgrep-regexp regex (projectile-acquire-root)))

(use-package ripgrep)
(use-package swiper
  :commands swiper)

(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)

(defun fi/my-org-latex-yas ()
  "Activate org and LaTeX yas expansion in org-mode buffers."
  (yas-minor-mode)
  (yas-activate-extra-mode 'latex-mode))

(add-hook 'org-mode-hook #'fi/my-org-latex-yas)
(use-package super-save
  :config
  (setq super-save-remote-files nil)
  (setq super-save-exclude '("*unsent mail*"))
  (setq auto-save-default nil)
  (super-save-mode 1))
(setq-default ispell-program-name "aspell")
(defun fi/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
(add-hook 'org-mode-hook #'fi/org-ispell)
(add-hook 'org-mode-hook 'flyspell-mode)
(use-package flyspell-correct
  :after flyspell)

(use-package avy
  :commands avy-goto-char-2
  :general (fi/leader "f" 'avy-goto-char-2))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs)

(use-package vundo
  :commands vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))
(use-package zoom
  :config
  (zoom-mode 1))

(provide 'fi-editing)
