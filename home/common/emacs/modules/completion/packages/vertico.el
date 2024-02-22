(use-package vertico
  :config
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 17)
  (vertico-resize nil)
  (vertico-cycle t)
  :config
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (vertico-mode))

;; (use-package vertico-posframe
;;   :after vertico
;;   :custom
;;   (vertico-posframe-border-width 1)
;;   :config
;;   (vertico-posframe-mode 1))

;; (use-package vertico-mouse
;;   :after vertico
;;   :config
;;   (vertico-mouse-mode))

;;; Utils
;; Save persistently the history of vertico
(use-package savehist
  :config
  (savehist-mode))
