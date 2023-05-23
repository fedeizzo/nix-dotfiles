;;; fi-checkers.el --- Checkers config

;;; Commentary:
;; Spellers

;;; Code:
(fi/load-package-config CHECKERS-MODULE-PATH "ispell.el")
;; (fi/load-package-config CHECKERS-MODULE-PATH "flycheck.el")
(fi/load-package-config CHECKERS-MODULE-PATH "flyspell.el")

(provide 'fi-checkers)

;;; fi-checkers.el ends here
