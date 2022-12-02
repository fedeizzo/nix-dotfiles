(defun fi/open-ledger ()
  (interactive)
  (find-file "~/docs/finance/finance.journal"))
(defun hledger/custom-run-command (command)
  "Run a custom hledger COMMAND."
  (interactive (list (completing-read "jdo> "
                                      hledger/hledger-custom-jcompletions)))
  (pcase command
    ("is-years" (hledger-run-command "--pretty=yes -s is -Y"))
    ("expenses-years" (hledger-run-command "--pretty=yes -s bal -t expenses --sort-amount -Y -2 -A"))
    ("expenses-years%" (hledger-run-command "--pretty=yes -s bal -t expenses -% --sort-amount -Y -2 -A"))
    ("total" (hledger-run-command "--pretty=yes -s balancesheetequity \'not:opening\'"))
    ("budget" (hledger-run-command "--pretty=yes -s bal --auto budget -t"))
    ("budget-year" (hledger-run-command "--pretty=yes -s bal --auto budget -t -p \'this year\' -M"))
    ("is-year" (hledger-run-command "--pretty=yes -s is -t -p \'this year\' -M"))
    ("is-month" (hledger-run-command "--pretty=yes -s is -t -p \'this month\'"))
    ("is-months" (hledger-run-command "--pretty=yes -s is -t -M -b 'in -3 months' -e 'this month' -A"))
    )
  )
(general-nmap "C-c j" 'fi/open-ledger)
(use-package hledger-mode
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :preface
  (defun hledger/next-entry ()
    "Move to next entry and pulse."
    (interactive)
    (hledger-next-or-new-entry)
    (hledger-pulse-momentary-current-entry))

  (defface hledger-warning-face
    '((((background dark))
       :background "Red" :foreground "White")
      (((background light))
       :background "Red" :foreground "White")
      (t :inverse-video t))
    "Face for warning"
    :group 'hledger)

  (defun hledger/prev-entry ()
    "Move to last entry and pulse."
    (interactive)
    (hledger-backward-entry)
    (hledger-pulse-momentary-current-entry))

  (defun hledger/format ()
    "Format an hledger buffer with align"
    (interactive)
    (align (point-min) (point-max)))

  (require 'align)
  (add-to-list 'align-rules-list
               `(hledger-accounts
                 (regexp . ,(rx (+ space)
                                (+? anything)
                                (group-n 1 space (+ space)
                                         (? ?-)
                                         (+ digit)
                                         (? ?.)
                                         (* digit))))
                 (group . 1)
                 (spacing . 2)
                 (justify . t)
                 (separate . entire)
                 (modes . '(hledger-mode))))
  :config
  (setq
   hledger-jfile "~/docs/finance/finance.journal"
   hledger-currency-string "€"
   hledger-top-income-account "revenue"
   hledger-ratios-income-accounts "revenue"
   hledger-year-of-birth 1999
   hledger-life-expectancy 80
   hledger-extra-args "")
  (setq hledger/hledger-custom-jcompletions
        '("is-years" "expenses-years"  "expenses-years%" "total" "budget" "budget-year" "is-year" "is-month" "is-months"))
  (add-hook 'hledger-mode-hook (lambda () (add-hook 'before-save-hook 'hledger/format nil 'local)))
  (require 'hledger-input)
  (general-nmap "C-c j" 'hledger-run-command)
  (general-nmap :keymaps 'hledger-mode-map "C-c e" 'hledger-jentry)
  (general-nmap :keymaps 'hledger-mode-map "n" 'hledger/next-entry)
  (general-nmap :keymaps 'hledger-mode-map "N" 'hledger/prev-entry)
  (add-to-list 'company-backends 'hledger-company))


;; (use-package flycheck-hledger
;;   :after (flycheck hledger-mode)
;;   :hook hledger-mode
;;   :config
;;   (setq flycheck-hledger-strict t))

(provide 'fi-money)
