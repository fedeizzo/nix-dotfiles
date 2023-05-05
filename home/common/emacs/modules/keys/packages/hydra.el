(use-package hydra)
(use-package hydra-posframe
  :load-path "~/.config/emacs/hydra-posframe"
  :custom
  (hydra-posframe-border-width 1)
  :hook (after-init . hydra-posframe-mode))
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
