;;; fi-keys.el --- TODO

;;; Commentary:

;;; Code:
;; (fi/load-package-config KEYS-MODULE-PATH "evil.el")
;; (fi/load-package-config KEYS-MODULE-PATH "general.el")
(fi/load-package-config KEYS-MODULE-PATH "hydra.el")
(fi/load-package-config KEYS-MODULE-PATH "meow.el")
(fi/load-package-config KEYS-MODULE-PATH "which-key.el")

;; Keys
;; No question after killing a buffer (kill-buffer asks you which buffer to switch to)
(bind-key "C-x k" #'kill-this-buffer)

(provide 'fi-keys)

;;; fi-keys.el ends here
