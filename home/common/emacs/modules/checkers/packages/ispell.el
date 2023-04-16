;;; ispell.el --- Ispell configuration

;;; Commentary:
;; ispell package check the spelling of every word with the aspell program

;;; Code:
(use-package ispell
  :custom
  (ispell-program-name "aspell" "Specifies the binary used to check word spell"))

;;; ispell.el ends here
