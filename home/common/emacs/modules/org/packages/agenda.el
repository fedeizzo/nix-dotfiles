;;; agenda.el --- Org agenda configuration

;;; Commentary:
;; org agenda configuration

;;; Code:

;; Require
(require 'org-protocol)

;; Const and var
(defconst inbox-file "inbox.org" "Agenda inbox file.")
(defconst agenda-file "agenda.org" "Agenda agenda file.")
(defconst tasks-file "tasks.org" "Agenda tasks file.")

(setq
 org-agenda-files (list inbox-file agenda-file tasks-file)
 org-agenda-window-setup 'current-window ; open agenda in same window
 org-agenda-restore-windows-after-quit t
 org-refile-targets '(("~/zettelkasten/tasks.org" :maxlevel . 1))
 org-agenda-tags-column 1
 org-agenda-block-separator ""
 org-log-done 'time
 org-agenda-start-with-log-mode t
 )

(setq org-capture-templates
      `(("i" "Inbox" entry (file "inbox.org")
	 ,(concat "* TODO %?\n"
                  "/Entered on/ %U")
	 )))

;; Functions
(defun fi/open-inbox-file ()
  "Open inbox file."
  (interactive)
  (find-file (concat org-directory "/" inbox-file)))

(defun fi/open-agenda-file ()
  "Open agenda file."
  (interactive)
  (find-file (concat org-directory "/" agenda-file)))

(defun fi/refile-inbox-element ()
  "Refile the org element under the cursor."
  (interactive)
  (org-set-effort)
  (org-set-tags-command)
  (org-priority)
  (org-refile))

;; Save the corresponding buffers
(defun fi/save-agenda-files ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda ()
			 (when (member (buffer-file-name) org-agenda-files)
			   t)))
  (message "Saving org-agenda-files buffers... done"))

;; Add it after refile
(advice-add 'org-refile :after
            (lambda (&rest _)
              (fi/save-agenda-files)))

;; Keybinds
(evil-global-set-key 'normal (kbd "C-c c") #'org-capture)
(evil-global-set-key 'normal (kbd "C-c a") #'org-agenda)
(evil-global-set-key 'normal (kbd "C-c i") #'fi/open-inbox-file)
(evil-global-set-key 'normal (kbd "C-c e") #'fi/open-agenda-file)
(evil-global-set-key 'normal (kbd "C-c r") #'fi/refile-inbox-element)

;; Habits
(require 'org-habit)
(setq org-log-into-drawer t)


(setq org-agenda-custom-commands
      '(("g" "GTD"
	 (
	  (agenda ""
		  ((org-agenda-clockreport-parameter-plist
		    '(
		      :link t
		      :maxlevel 2
		      :properties ("Effort")
		      :fileskip0 t
		      :tcolumns 1
		      :formula "$5=$2-$4;T"
		      ))))
	  (todo "TODO"
		((org-agenda-files (list inbox-file))
		 (org-agenda-prefix-format '((todo . " ")))
		 (org-agenda-use-time-grid t)
		 (org-agenda-overriding-header "\nINBOX\n")))
	  (todo "TODO"
		((org-agenda-files (list tasks-file))
		 (org-agenda-prefix-format '((todo . " [%e] ")))
		 (org-agenda-overriding-header "TODO\n")
		 (org-agenda-tags-column 1)))))))
;;; agenda.el ends here
