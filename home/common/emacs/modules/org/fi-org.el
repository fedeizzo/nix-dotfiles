;;; fi-org.el --- Org-mode configuration

;;; Commentary:
;;

;;; Code:
(defun fi/org-mode-setup ()
  "Org-mode setup hook."
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :custom-face
  (org-level-1 ((t (:height 1.05))))
  (org-level-2 ((t (:height 1.04))))
  (org-level-3 ((t (:height 1.03))))
  (org-level-4 ((t (:height 1.02))))
  (org-level-5 ((t (:height 1.01))))
  :custom
  (org-ellipsis " …"                         "Symbol used when a heading is closed")
  (org-directory "~/zettelkasten/00-agenda"  "Org folder")
  (org-hide-emphasis-markers t               "Italic and bold prefix/suffix hidden")
  (org-tags-column 1                         "Tag on the right of the heading")
  (org-cycle-separator-lines 2               "empty line between sections")
  (org-use-tag-inheritance nil               "Tags are not inherited")
  (org-use-property-inheritance t            "Property are inherited")
  (org-return-follows-link t                 "Use RET to follow link")
  (org-indirect-buffer-display 'other-window "Tab on a task expand it in a new window")
  (org-confirm-babel-evaluate nil            "Don't ask confirmation for babel evaluation")
  (org-src-window-setup 'current-window      "Babel code opened in same window")
  ;; (org-latex-create-formula-image-program 'dvisvgm "https://stackoverflow.com/questions/30151338")
  (org-image-actual-width nil)
  (org-startup-with-latex-preview t)
  :hook
  (org-mode . fi/org-mode-setup)
  (org-mode . org-fragtog-mode)
  ;; (org-mode . org-num-mode)
  ;; (org-mode . org-outer-indent-mode)
  )
;; (org-mode . (lambda ()
;; 		(setq-local electric-pair-inhibit-predicate
;; 			    `(lambda (c)
;; 			       (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
(use-package org-contrib)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("li" . "src lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

(fi/load-package-config ORG-MODULE-PATH "agenda.el")
(fi/load-package-config ORG-MODULE-PATH "org-modern.el")
(fi/load-package-config ORG-MODULE-PATH "org-outer-indent.el")
(fi/load-package-config ORG-MODULE-PATH "latex.el")

(defun fi/dummy-org-download-annotate-function (link)
  ""
  "#+ATTR_ORG: :width 250px\n#+ATTR_LATEX: :width 250px :placement [H] \n#+CAPTION: \n#+NAME: \n"

  )

(use-package org-download
  :after org
  :config
  (setq org-downlaod-screenshot-method (if ON-MACBOOK
					   "screencapture -i -c -P"
					 "grim -g \"$(slurp)\" - | swappy -f -"))
  (setq org-download-annotate-function
        #'fi/dummy-org-download-annotate-function)
  (setq org-downlaod-image-dir "./img"))
(setq org-link-frame-setup '((file . find-file)))


;; (setq
;;  org-directory "~/org"
;;  fi/org-agenda-inbox-file (concat org-directory "/inbox.org")
;;  fi/org-agenda-work-file (concat org-directory "/work.org")
;;  fi/org-agenda-uni-file (concat org-directory "/uni.org")
;;  fi/org-agenda-personal-file (concat org-directory "/personal.org")
;;  fi/org-agenda-habits-file (concat org-directory "/habits.org")
;;  fi/org-agenda-files (list
;; 		      fi/org-agenda-work-file
;; 		      fi/org-agenda-uni-file
;; 		      fi/org-agenda-personal-file))
(use-package org-remark
  :after org
  :config
  (org-remark-global-tracking-mode t)
  (setq org-remark-notes-file-name (lambda ()
				     (concat "~/zettelkasten/org-remark/"
					     (file-name-base (org-remark-notes-file-name-function))
					     ".org"))))

(use-package org-cliplink)
;; (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "DOING(c)" "HOLD(h)" "|" "DONE(d)")))
;; (setq org-todo-keywords-for-agenda '((sequence "TODO(t)" "NEXT(n)" "DOING(c)" "HOLD(h)" "|" "DONE(d)")))
;; (setq org-todo-state-tags-triggers '((sequence "TODO(t)" "NEXT(n)" "DOING(c)" "HOLD(h)" "|" "DONE(d)")))
;; (setq org-agenda-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "DOING(c)" "HOLD(h)" "|" "DONE(d)")))
;; (setq-default org-enforce-todo-dependencies t)

;; (setq org-tag-alist
;;       '(("@work" . ?w)
;;         ("@uni" . ?u)
;;         ("@home" . ?h)))

;; (setq
;;  org-agenda-files (list org-directory)
;;  org-agenda-breadcrumbs-separator " ❱ "
;;  org-agenda-block-separator "──────────"
;;  org-agenda-include-inactive-timestamps t
;;  org-agenda-log-mode-items '(closed clock state)
;;  org-agenda-start-with-log-mode '(closed clock state)
;;  org-agenda-tags-column 0
;;  org-columns-default-format "%60ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock) %TAGS"
;;  )
;; (setq org-agenda-custom-commands
;;       '(
;;         (" " "Agenda"
;;          (
;;           (agenda ""
;;                   ((org-agenda-overriding-header  " This week")))
;;           (todo "TODO"
;;                 ((org-agenda-overriding-header " To process / Inbox")
;;                  (org-agenda-prefix-format "%b")
;;                  (org-super-agenda-groups nil)
;;                  (org-agenda-files (list fi/org-agenda-inbox-file))))
;;           (todo "NEXT"
;;                 ((org-agenda-overriding-header " NEXT")
;;                  (org-agenda-prefix-format "")
;;                  (org-super-agenda-groups '((:auto-category t)))))
;;           (todo "DOING"
;;                 ((org-agenda-overriding-header " DOING")
;;                  (org-agenda-prefix-format "")
;;                  (org-super-agenda-groups '((:auto-category t)))))
;;           (todo "HOLD"
;;                 ((org-agenda-overriding-header " HOLD")
;;                  (org-agenda-prefix-format "")
;;                  (org-super-agenda-groups '((:auto-category t)))))
;;           (todo "TODO"
;;                 ((org-agenda-overriding-header " TODO")
;;                  (org-agenda-prefix-format "")
;;                  (org-agenda-files fi/org-agenda-files)
;;                  (org-super-agenda-groups '((
;;                                              :auto-category t
;;                                              :auto-priority t
;;                                              )))))
;;           )
;;          )
;;         ))

;; (defun fi/refile-task ()
;;   (interactive)
;;   (setq org-refile-targets '((fi/org-agenda-files :maxlevel . 1)))
;;   (org-agenda-set-tags)
;;   (org-agenda-priority)
;;   (org-agenda-set-effort)
;;   (org-agenda-refile)
;;   (setq org-refile-targets nil)
;;   (org-agenda-redo-all))

(setq org-agenda-window-setup 'current-window)

;; (pretty-hydra-define agenda-hydra-main (:color blue :title "Agenda" :quit-key "q")
;;   ("Task"
;;    (("r" fi/refile-task "refile")
;;     ("t" org-agenda-todo "todo status")
;;     ("i" org-agenda-clock-in "clock in")
;;     ("o" org-agenda-clock-out "clock out")
;;     ))
;;   )
;; (defun fi/switch-to-agenda ()
;;   (interactive)
;;   (org-agenda nil " "))

;; (fi/leader "a" #'fi/switch-to-agenda)
;; (define-key org-agenda-mode-map (kbd "SPC") 'agenda-hydra-main/body)


;; (setq org-default-notes-file fi/org-agenda-inbox-file)
;; (defun transform-square-brackets-to-round-ones(string-to-transform)
;;   "Transforms [ into ( and ] into ), other chars left unchanged."
;;   (concat
;;    (mapcar (lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))
;; (setq org-capture-templates
;;       '(
;;         ("t" "To-do task to process" entry (file+headline fi/org-agenda-inbox-file "Task") "* TODO%?\n" :empty-lines 1)
;;         ("r" "Thing to read" entry (file+headline fi/org-agenda-inbox-file "Manual") "* TODO %(org-cliplink-capture)\n" :empty-lines 1 :immediate-finish t)
;;         ("p" "Protocol text" entry (file+headline fi/org-agenda-inbox-file "Protocol") "* TODO %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?" :empty-lines 1 :immediate-finish t)
;;         ("L" "Protocol link" entry (file+headline fi/org-agenda-inbox-file "Protocol") "* TODO [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n#+CREATED: [%<%Y-%m-%d %a %H:%M:%S>]\n" :empty-lines 1 :immediate-finish t)
;;         ))

(use-package async)

(require 'ob-python)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((hledger . t)
   (gnuplot .t)
   (dot . t)
   (shell . t)
   ;; other languages..
   ))

(use-package ox-epub)
(use-package ox-hugo
  :after ox
  :config
  (setq
   org-hugo-base-dir "~/personalProject/hugo-website"
   org-hugo-section "notes"
   org-hugo-front-matter-format "yaml"))

(require 'ox-latex)
(setq org-latex-listings 't)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(add-to-list 'org-latex-packages-alist '("" "svg"))
(use-package ox-awesomecv
  :load-path "~/.config/emacs/org-cv"
  :init (require 'ox-awesomecv))

(use-package ox-hugocv
  :load-path "~/.config/emacs/org-cv"
  :init (require 'ox-hugocv))
(defun fi/pdf-resume ()
  (interactive)
  (let ((old-pdf-value org-latex-pdf-process)
        (old-compiler org-latex-compiler))
    (setq org-latex-pdf-process '("latexmk -f -pdf -%latex -interaction=nonstopmode -bibtex -output-directory=%o %f"))
    (setq org-latex-compiler "xelatex")
    (org-export-to-file 'awesomecv "cv.tex")
    (org-latex-compile "cv.tex")
    (setq org-latex-pdf-process old-pdf-value)
    (setq org-latex-compiler old-compiler)))

(defun fi/hugo-resume ()
  (interactive)
  (org-export-to-file 'hugocv "resume.md"))
(add-hook 'org-mode-hook (lambda ()
                           (setq-local time-stamp-active t
				       time-stamp-line-limit 18
				       time-stamp-start "^#\\+LAST_MODIFIED: [ \t]*"
				       time-stamp-end "$"
				       time-stamp-format "\[%Y-%m-%d %a %H:%M:%S\]")
                           (add-hook 'before-save-hook 'time-stamp nil 'local)))
(defun fi/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package org-ref)

(use-package visual-fill-column
  :defer t
  :hook (org-mode . fi/org-mode-visual-fill))
(setq org-use-sub-superscripts "{}")
(setq org-startup-with-inline-images t)
(use-package org-noter
  :config
  (setq org-noter-auto-save-last-location t)
  (setq org-noter-notes-search-path '("~/zettelkasten/noter"))
  (setq org-noter-notes-window-behavior '(start scroll))
  )
;; (pretty-hydra-define zetteldesk-add-hydra (:color blue :title "Add to Zetteldesk" :quit-key "q")
;;   ("Org-Roam"
;;    (("n" zetteldesk-add-node-to-desktop "Add Node")
;;     ("b" zetteldesk-add-backlinks-to-desktop "Add Node + All its backlinks"))
;;    "Other"
;;    (("a" zetteldesk-add-to-desktop "Add Buffer"))))

;; (pretty-hydra-define zetteldesk-remove-hydra (:color blue :title "Remove from Zetteldesk" :quit-key "q")
;;   ("Org-Roam"
;;    (("n" zetteldesk-remove-node-from-desktop "Remove Node")
;;     ("b" zetteldesk-remove-backlinks-from-desktop "Remove Node + All its backlinks"))
;;    "Other"
;;    (("r" zetteldesk-remove-from-desktop "Remove Buffer"))))

;; (pretty-hydra-define zetteldesk-insert-hydra (:color blue :title "Insert from the Zetteldesk" :quit-key "q")
;;   ("Org-Roam"
;;    (("n" zetteldesk-insert-node-contents-without-link "Node Contents in *zetteldesk-scratch")
;;     ("a" fi/zetteldesk-insert-all-nodes-contents-without-link  "All nodes Contents in *zetteldesk-scratch*")
;;     ("B" fi/zetteldesk-insert-all-nodes-contents-current-buffer  "All nodes Contents in current buffer in *zetteldesk-scratch*"))
;;    "Supplementary Material to *zetteldesk-scratch*"
;;    (("p" zetteldesk-insert-link-to-pdf "Link to PDF"))))

;; (pretty-hydra-define zetteldesk-main-hydra (:color blue :title "Zetteldesk Hydra" :quit-key "q")
;;   ("Filter Functions"
;;    (("n" zetteldesk-node-find "Find Zetteldesk Node"))

;;    "Add/Remove Hydras"
;;    (("r" zetteldesk-remove-hydra/body "Run the Removing Hydra")
;;     ("a" zetteldesk-add-hydra/body "Run the Adding Hydra"))

;;    "Inserting Things and *zetteldesk-scratch*"
;;    (("s" zetteldesk-switch-to-scratch-buffer "Switch to *zetteldesk-scratch*")
;;     ("i" zetteldesk-insert-hydra/body "Run the Insert Hydra"))))

;; (fi/leader "u" 'zetteldesk-main-hydra/body)
;; line number
(column-number-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; add hook to disable line numbers in org-mode
(dolist (mode '(org-mode-hook
                org-agenda-mode-hook
                shell-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(set-default-coding-systems 'utf-8)
(defun fi/switch-last-buffer (arg)
  "Switch to last visited buffer.

      Use prefix arg to specify the order of the buffer, most to least
      recent. Does not update buffer list, so, for example, after moving to the
      nth most recent buffer, use unprefixed command to get back to original
      buffer."
  (interactive "P")
  (switch-to-buffer (if arg
                        (elt (buffer-list) arg)
		      (other-buffer))
                    t))

;; (defun fi/zetteldesk-add-current-buffer-to-desktop ()
;;   "Add current buffer to zetteldek desktop"
;;   (interactive)
;;   (zetteldesk-add-to-desktop (current-buffer)))

(defun fi/open-file-new-split ()
  "Open at mouse in other window"
  (interactive)
  (let ((org-link-frame-setup '((file . find-file-other-window))))
    (org-open-at-point)))

(global-set-key [C-down-mouse-1] 'fi/open-file-new-split)
(global-set-key [mouse-8] 'fi/switch-last-buffer)
;; (global-set-key [mouse-9] 'fi/zetteldesk-add-current-buffer-to-desktop)

(defun fi/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
(add-hook 'org-mode-hook #'fi/org-ispell)
(add-hook 'org-mode-hook 'flyspell-mode)

;; (use-package org-fc
;;   :load-path "~/.config/emacs/org-fc")
;; :custom
;; (org-fc-directories "~/zettelkasten")
;; (org-fc-review-history-file "~/zettelkasten/org-fc-reviews.tsv"))

(fi/load-package-config ORG-MODULE-PATH "org-roam.el")

(provide 'fi-org)

;;; fi-org.el ends here
