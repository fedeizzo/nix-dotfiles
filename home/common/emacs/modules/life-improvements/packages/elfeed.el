;;; elfeed.el --- Elfeed configuration

;;; Commentary:
;;

;;; Code:

(use-package elfeed)

(use-package elfeed-org
  :after elfeed
  :custom
  (elfeed-db-directory "~/zettelkasten/.elfeed"))

(use-package elfeed-summary
  :after elfeed
  :custom
  (rmh-elfeed-org-files '("~/zettelkasten/elfeed.org"))
  :init
  (elfeed-org))

(defun fi/open-elfeed-org ()
  (interactive)
  (find-file "~/zettelkasten/elfeed.org"))

(fi/leader
  "ee" #'elfeed
  "ex" #'elfeed-db-unload
  "es" #'elfeed-summary
  "eu" #'elfeed-update
  "eo" #'fi/open-elfeed-org)

;;; elfeed.el ends here
