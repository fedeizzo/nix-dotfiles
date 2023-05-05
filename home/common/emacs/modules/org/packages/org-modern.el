;;; org-modern.el --- TODO

;;; Commentary:
;; TODO

;;; Code:
(use-package org-modern
  :custom
  (org-modern-star '("#" "##" "###" "####" "#####"))
  (org-modern-hide-stars 'leading)
  (org-modern-timestamp nil)
  (org-modern-table t)
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 1)
  (org-modern-list '((?+ . "•") (?- . "•") (?* . "•")))
  (org-modern-horizontal-rule t)
  (org-modern-todo nil)
  (org-modern-tag nil)
  (org-modern-block t)
  (org-modern-block-name t)
  (org-modern-block-fringe t)
  (org-modern-keyword nil)
  (org-pretty-entities nil)
  (org-modern-statistics nil))

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;;; org-modern.el ends here
