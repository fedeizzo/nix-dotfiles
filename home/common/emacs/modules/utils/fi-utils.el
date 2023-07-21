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

(defun fi/hydra-title-factory-fileicon (icon title)
  (s-concat (all-the-icons-fileicon icon :v-adjust -0.2 :height 1.5) " " title))

(defun fi/hydra-title-factory-faicon (icon title)
  (s-concat (all-the-icons-faicon icon :v-adjust -0.2 :height 1.5) " " title))


;;; Update functions
(defun fi/kill-buffer-and-window-delete ()
  "Kill this buffer and then delete the window."
  (interactive)
  (kill-this-buffer)
  (evil-window-delete))

(defun fi/update-nixos ()
  "Update nixos configuration."
  (interactive)
  (with-output-to-temp-buffer "*nixos-update*"
    (shell-command "cd ~/nix-dotfiles && ./install.sh &"
                   "*nixos-update*"
                   "*nixos-update*")
    (pop-to-buffer "*nixos-update*")
    (evil-force-normal-state)
    (evil-local-set-key 'normal (kbd "q") 'fi/kill-buffer-and-window-delete)))

(defun fi/load-emacs-module ()
  "Load emacs user defined module."
  (interactive)
  (load (completing-read "Select el file to load:"
                         (directory-files-recursively
                          "~/nix-dotfiles/home/common/emacs/modules/" ".*el$"))))
(defun run-in-vterm (command)
  "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))

(defun dd/workspace-background-connection()
  "Connect to dd workspace."
  (interactive)
  (run-in-vterm "TERM=xterm-256color ssh workspace-federicoizzo"))

(defun dd/staging-db-connection()
  "Connect to staging PostgreSQL database."
  (interactive)
  (run-in-vterm "~/.scripts/s8s-db-conn"))

(defun fi/clean-tilde-emacs-file()
  "Clean temp files ending with ~."
  (interactive)
  (projectile-with-default-dir (projectile-acquire-root)
    (shell-command "fd --regex '.*~$' --exec rm")))

;; fd --regex '.*~$'
;;; Hooks
;; Sort package loading times ascending
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (sort fi/packages-loading-time (lambda (a b) (> (car (cdr a)) (car (cdr b)))))))

(provide 'fi-utils)

;;; fi-utils.el ends here
