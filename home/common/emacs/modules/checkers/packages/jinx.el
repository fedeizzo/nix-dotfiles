;;; jinx.el --- Package to check the spelling

;;; Commentary:
;;

;;; Code:
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))
(setq jinx-languages "en")
;;; jinx.el ends here
