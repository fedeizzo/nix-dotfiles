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
 org-refile-targets '(("~/zettelkasten/00-agenda/tasks.org" :maxlevel . 1))
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
  (org-agenda-set-effort)
  (org-agenda-set-tags)
  (org-agenda-priority)
  (org-agenda-refile))

;; Save the corresponding buffers
(defun fi/save-agenda-files ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (dolist (buf-name (mapcar (lambda (el) (concat org-directory "/" el)) org-agenda-files))
    (with-current-buffer (get-file-buffer buf-name)
      (save-some-buffers t)))
  (message "Saving org-agenda-files buffers... done"))

;; Add it after refile
(advice-add 'org-refile :after
            (lambda (&rest _)
              (fi/save-agenda-files)))

;; Keybinds
;; (evil-global-set-key 'normal (kbd "C-c c") #'org-capture)
;; (evil-global-set-key 'normal (kbd "C-c a") #'org-agenda)
;; (evil-global-set-key 'normal (kbd "C-c i") #'fi/open-inbox-file)
;; (evil-global-set-key 'normal (kbd "C-c e") #'fi/open-agenda-file)
;; (evil-global-set-key 'normal (kbd "C-c r") #'fi/refile-inbox-element)

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

(defun fi/open-agenda-in-gtd-mode ()
  "Open agenda in gtd mode."
  (interactive)
  (org-agenda nil "g"))

(defun fi/org-capture-inbox ()
  "Call `org-capture` with the right key for inbox."
  (interactive)
  (org-capture nil "i"))

(defun fi/archive-all-done ()
  "Archive all done tasks."
  (interactive)
  (move-end-of-line nil)
  (org-insert-heading nil)
  (org-archive-all-old))

(defun fi/push-agenda-files ()
  "Push agenda files."
  (interactive)
  (with-output-to-temp-buffer "*push-agenda-files*"
    (shell-command (concat "cd "org-directory " && git add *.org && git commit -m 'update org agenda' && git push")
                   "*push-agenda-files*"
                   "*push-agenda-files*")
    (pop-to-buffer "*push-agenda-files*")
    (local-set-key (kbd "q") 'fi/kill-buffer-and-window-delete)))



(pretty-hydra-define fi/org-agenda-hydra (:color blue :title "  Agenda" :quit-key "q")
  ("Outside the agenda"
   (("o" #'fi/open-agenda-in-gtd-mode "open agenda")
    ("c" #'fi/org-capture-inbox "org capture inbox")
    ("d" #'org-roam-dailies-goto-date "go/create daily note")
    ("s" #'fi/save-agenda-files "save agenda files")
    ("p" #'fi/push-agenda-files "push agenda files")
    )
   "Inside the agenda"
   (("r" #'fi/refile-inbox-element "refile element")
    ("a" #'org-archive-subtree "archive done")))
  )

(bind-key "C-q" #'fi/org-agenda-hydra/body)


;;; agenda.el ends here
