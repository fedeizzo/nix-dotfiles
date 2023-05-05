;;; fi-utils.el --- Personal utility stuff

;;; Commentary:
;; Utility const, functions, etc.

;;; Code:

;;; Requires
(require 'benchmark) ;; used to have benchmark-call function

;;; Consts
;; List of modules
(defconst MODULES-PATH "~/.config/emacs/modules")
(defconst COMPLETION-MODULE-PATH (concat MODULES-PATH "/completion"))
(defconst UI-MODULE-PATH (concat MODULES-PATH "/ui"))
(defconst CHECKERS-MODULE-PATH (concat MODULES-PATH "/checkers"))
(defconst KEYS-MODULE-PATH (concat MODULES-PATH "/keys"))
(defconst UTILS-MODULE-PATH (concat MODULES-PATH "/utils"))
(defconst PROG-MODULE-PATH (concat MODULES-PATH "/prog"))
(defconst LIFE-IMPROVEMENTS-MODULE-PATH (concat MODULES-PATH "/life-improvements"))
(defconst ORG-MODULE-PATH (concat MODULES-PATH "/org"))


;;; Variables
;; Holds a list of pairs packages <-> loading times
(defvar fi/packages-loading-time '())

;;; Functions
(defun fi/load-package-config (module package-name)
  "Helper function for loading a package.
package as PACKAGE-NAME, module as MODULE."
  (let ((elapsed-time
	 (benchmark-elapse (load (expand-file-name(concat module "/packages/" package-name))))))
    (add-to-list 'fi/packages-loading-time (list package-name elapsed-time) t)))

(defun fi/insert-accented-e ()
  "Insert accented e."
  (interactive)
  (insert "é"))

(defun fi/insert-circonflex-e ()
  "Insert accented e."
  (interactive)
  (insert "ê"))

(defun fi/reload-fi-configs ()
  "Reload personal config."
  (interactive)
  (load-file "~/.config/emacs/init.el"))

(defun fi/get-project-filepath ()
  "Return the filename wrt to the current project."
  (interactive)
  (kill-new (file-relative-name buffer-file-name
				(projectile-project-root))))

;;; Hooks
;; Sort package loading times ascending
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (sort fi/packages-loading-time (lambda (a b) (> (car (cdr a)) (car (cdr b)))))))

(provide 'fi-utils)

;;; fi-utils.el ends here
