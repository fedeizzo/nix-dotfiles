;;; python.el --- Python tools configuration

;;; Commentary:
;;

;;; Code:

(use-package python-pytest)

(use-package pyvenv)

(with-eval-after-load 'flycheck
  (add-to-list 'flycheck-python-mypy-ini "mypy_loose.ini")
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

;;; python.el ends here
