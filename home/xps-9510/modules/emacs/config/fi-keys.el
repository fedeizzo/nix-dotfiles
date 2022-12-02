(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion (kbd "<down>") 'evil-next-visual-line)
  (evil-global-set-key 'motion (kbd "<up>") 'evil-previous-visual-line)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1))

(use-package hydra)
;; (use-package hydra-postframe)
(use-package major-mode-hydra
  :bind
  ("M-SPC" . major-mode-hydra))
(require 'hydra)

(major-mode-hydra-define emacs-lisp-mode nil
  ("Eval"
   (("b" eval-buffer "buffer")
    ("e" eval-defun "defun")
    ("r" eval-region "region"))
   "REPL"
   (("I" ielm "ielm"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("d" describe-foo-at-point "thing-at-pt")
    ("f" describe-function "function")
    ("v" describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))))

(use-package general
  :config
  (general-evil-setup t)
  (general-override-mode 1)
  (general-create-definer fi/leader
    :states 'normal
    :keymaps '(override)
    :prefix "SPC")
  (fi/leader
    "s" 'save-buffer
    ;; origami
    "zc" 'origami-close-node
    "zC" 'origami-close-all-nodes
    "zo" 'origami-open-node
    "zO" 'origami-open-all-nodes
    "zr" 'origami-close-node-recursively
    "zR" 'origami-open-node-recursively))

(provide 'fi-keys)
