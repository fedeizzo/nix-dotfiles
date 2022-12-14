(defun mu4e/custom-delete ()
  "Custom mark function to move to deleted folder"
  (interactive)
  (mu4e-mark-set 'move "/deleted")
  (mu4e-mark-execute-all t)
  (mu4e-mark-set 'read)
  (mu4e-mark-execute-all t)
  (mu4e-headers-next))

(use-package mu4e
  :hook
  (mu4e-main-mode . (lambda () (display-line-numbers-mode 0)))
  (mu4e-headers-mode . (lambda () (display-line-numbers-mode 0)))
  :general
  (fi/leader "m" 'mu4e)
  :config
  (general-nmap mu4e-headers-mode-map "d" 'mu4e/custom-delete)
  (setq
   user-full-name "Federico Izzo"
   user-mail-address "federico@fedeizzo.dev")
  (setq
   mu4e-change-filenames-when-moving t
   mu4e-get-mail-command "mbsync -a"
   mu4e-maildir "~/.mail"
   mu4e-drafts-folder "/Drafts"
   mu4e-sent-folder "/Sent"
   mu4e-refile-folder "/All Mail"
   mu4e-trash-folder "/Trash"

   mu4e-maildir-shortcuts '(("/Inbox" . ?i)
			    ("/Drafts" . ?d)
			    ("/deleted" . ?x)))
  ;; (setq mu4e-contexts
  ;; 	`(,(make-mu4e-context
  ;; 	    :name "Personal"
  ;; 	    :enter-func (lambda () (mu4e-message "Entering personal context"))
  ;; 	    :leave-func (lambda () (mu4e-message "Leaving personal context"))
  ;; 	    :match-func (lambda (msg)
  ;; 			  (when (msg)
  ;; 			    (mu4e-message-contact-field-matches msg :to "federico@fedeizzo.dev")))
  ;; 	    :vars '((user-mail-address . "federico@fedeizzo.dev")
  ;; 		    (user-full-name . "Federico Izzo")
  ;; 		    (mu4e-compose-signature .(concat "Federico Izzo\n"))))
  ;; 	  ,(make-mu4e-context
  ;; 	    :name "University"
  ;; 	    :enter-func (lambda () (mu4e-message "Entering university context"))
  ;; 	    :leave-func (lambda () (mu4e-message "Leaving university context"))
  ;; 	    :match-func (lambda (msg)
  ;; 			  (when (msg)
  ;; 			    (mu4e-message-contact-field-matches msg :to "federico.izzo@studenti.unitn.it")))
  ;; 	    :vars '((user-mail-address . "federico.izzo@studenti.unitn.it")
  ;; 		    (user-full-name . "Federico Izzo")
  ;; 		    (mu4e-compose-signature .(concat "Federico Izzo\n"))))
  ;; 	  ,(make-mu4e-context
  ;; 	    :name "FirstGmail"
  ;; 	    :enter-func (lambda () (mu4e-message "Entering federico.izzo99 context"))
  ;; 	    :leave-func (lambda () (mu4e-message "Leaving federico.izzo99 context"))
  ;; 	    :match-func (lambda (msg)
  ;; 			  (when (msg)
  ;; 			    (mu4e-message-contact-field-matches msg :to "federico.izzo99@gmail.com")))
  ;; 	    :vars '((user-mail-address . "federico.izzo99@gmail.com")
  ;; 		    (user-full-name . "Federico Izzo")
  ;; 		    (mu4e-compose-signature . (concat "Federico Izzo\n"))))
  ;; 	  ,(make-mu4e-context
  ;; 	    :name "SecondGmail"
  ;; 	    :enter-func (lambda () (mu4e-message "Entering ozzi context"))
  ;; 	    :leave-func (lambda () (mu4e-message "Leaving ozzi context"))
  ;; 	    :match-func (lambda (msg)
  ;; 			  (when (msg)
  ;; 			    (mu4e-message-contact-field-matches msg :to "ozzi.ezzo@gmail.com")))
  ;; 	    :vars '((user-mail-address . "ozzi.ezzo@gmail.com")
  ;; 		    (user-full-name . "Federico Izzo")
  ;; 		    (mu4e-compose-signature . (concat "Federico Izzo\n"))))
  ;; 	  ))
  (setq sendmail-program "/usr/bin/msmtp"
	send-mail-function 'smtpmail-send-it
	message-sendmail-f-is-evil t
	message-sendmail-extra-arguments '("--read-envelope-from")
	message-send-mail-function 'message-send-mail-with-sendmail)


  )

;; ;; (use-package org-mime
;; ;;   :config
;; ;;   (setq org-mime-library 'mml))

(provide 'fi-mail)
