;;; meow.el --- meow configuration

;;; Commentary:
;; meow configuration

;;; Code:
(defun meow-negative-find ()
  (interactive)
  (let ((current-prefix-arg -1))
    (call-interactively 'meow-find)))

(defun meow-negative-till ()
  (interactive)
  (let ((current-prefix-arg -1))
    (call-interactively 'meow-till)))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   '("p" . projectile-hydra-main/body)
   '("d" . dap-hydra/body)
   '("o" . org-roam-hydra-main/body)
   '("w" . ace-window)
   '("t" . treemacs)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("F" . meow-negative-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . consult-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . meow-negative-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :custom
  (meow-expand-hint-remove-delay 2.0)
  (meow-visit-collect-min-length 3)
  :config
  (meow-setup)
  (meow-global-mode 1)
  (meow-thing-register 'arrow '(pair ("<") (">")) '(pair ("<") (">")))
  (add-to-list 'meow-char-thing-table '(?a . arrow)))

(setq meow-paren-keymap (make-keymap))
(meow-define-state paren
  "meow state for interacting with smartparens"
  :lighter " [P]"
  :keymap meow-paren-keymap)

;; meow-define-state creates the variable
(setq meow-cursor-type-paren 'hollow)

(meow-define-keys 'paren
  '("<escape>" . meow-normal-mode)
  '("l" . sp-forward-sexp)
  '("h" . sp-backward-sexp)
  '("j" . sp-down-sexp)
  '("k" . sp-up-sexp)
  '("n" . sp-forward-slurp-sexp)
  '("b" . sp-forward-barf-sexp)
  '("v" . sp-backward-barf-sexp)
  '("c" . sp-backward-slurp-sexp)
  '("u" . meow-undo))

(global-set-key (kbd "C-S-C") #'clipboard-kill-ring-save)
(global-set-key (kbd "C-S-V") #'clipboard-yank)

;;; meow.el ends here
