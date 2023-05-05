;;; corfu.el --- Corfu configuration

;;; Commentary:
;; Corfu is a completion package

;;; Code:

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-quit-no-match t)
  :init
  (global-corfu-mode)
  :config
  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
	#'command-completion-default-include-p)
  )

;; memorize selection history to show the list of completion sorted
(use-package corfu-history
  :after corfu
  :config
  (corfu-history-mode))

;; docs on the completion side
(use-package corfu-popupinfo
  :after corfu
  :custom
  (corfu-popupinfo-delay '(1.0 . 0.5))
  :config
  (corfu-popupinfo-mode))

(use-package corfu-info
  :after corfu-popupinfo)

;;; corfu.el ends here
