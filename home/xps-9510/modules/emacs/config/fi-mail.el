(defun fi/notmuch-choose-sender ()
  (cond
   ((string= (message-fetch-field "from") "Federico Izzo <federico.izzo99@gmail.com>")
    (setq message-sendmail-extra-arguments '("/home/fedeizzo/.mail/fedeizzo")))
   ((string= (message-fetch-field "from") "Federico Izzo <ozzi.ezzo@gmail.com>")
    (setq message-sendmail-extra-arguments '("/home/fedeizzo/.mail/ozzi")))))

(use-package notmuch
  :commands notmuch
  :hook
  (message-send . fi/notmuch-choose-sender)
  (notmuch-hello-mode . (lambda () (display-line-numbers-mode 0)))
  (notmuch-show-mode . (lambda () (display-line-numbers-mode 0)))
  :general
  (fi/leader "m" 'notmuch)
  :config
  (defun evil-collection-notmuch-search-toggle-delete ()
    (interactive)
    (notmuch-search-add-tag '("-inbox" "-unread" "-important" "+deleted"))
    (notmuch-tree-next-message))
  (setq
   message-required-mail-headers (remove' Message-ID message-required-mail-headers)
   message-kill-buffer-on-exit t
   message-send-mail-function #'message-send-mail-with-sendmail)
  (setq sendmail-error-reporting-interactive '()
        sendmail-error-reporting-non-interactive '()
        sendmail-program "~/nix-dotfiles/home/xps-9510/sources/send_email")

  (setq notmuch-init-file "~/.config/notmuch/default/config"
        notmuch-search-oldest-first nil
        notmuch-fcc-dirs nil
        notmuch-search-result-format '(("date" . "%12s ")
                                       ("count" . "%-7s ")
                                       ("authors" . "%-30s ")
                                       ("subject" . "%-72s ")
                                       ("tags" . "(%s)"))
        notmuch-archive-tags '("-inbox" "-unread" "+archive")
        notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
				 (:name "fedeizzo" :query "to:federico.izzo99@gmail.com and tag:inbox" :key "f")
                                 (:name "ozzi" :query "to:ozzi.ezzo@gmail.com and tag:inbox" :key "o")
                                 (:name "uni" :query "to:federico.izzo@studenti.unitn.it and tag:inbox" :key "u")
                                 (:name "sent" :query "tag:sent" :key "t")
                                 (:name "drafts" :query "tag:draft" :key "d")
                                 (:name "all mail" :query "*" :key "a"))
        notmuch-show-empty-saved-searches t
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches notmuch-hello-insert-alltags))
  ;; (evil-collection-define-key 'normal 'notmuch-tree-mode-map
  ;;   "u" 'fi/notmuch-toggle-inbox-tree)
  ;; (evil-collection-define-key 'normal 'notmuch-search-mode-map
  ;;   "u" 'fi/notmuch-toggle-inbox-show)
  )

;; (use-package org-mime
;;   :config
;;   (setq org-mime-library 'mml))

(provide 'fi-mail)
