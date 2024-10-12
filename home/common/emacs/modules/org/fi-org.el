;;; fi-org.el --- Org-mode configuration

;;; Commentary:
;;

;;; Code:
(defun fi/org-mode-setup ()
  "Org-mode setup hook."
  (auto-fill-mode 0)
  (visual-line-mode 1))

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
  (setq org-download-screenshot-method (if ON-MACBOOK
					   "screencapture -i -c -P"
					 "grim -g \"$(slurp)\" - | swappy -f -"))
  (setq org-download-annotate-function
        #'fi/dummy-org-download-annotate-function)
  (setq org-download-image-dir "./img"))
(setq org-link-frame-setup '((file . find-file)))


(use-package org-remark
  :after org
  :config
  (org-remark-global-tracking-mode t)
  (setq org-remark-notes-file-name (lambda ()
				     (concat "~/zettelkasten/org-remark/"
					     (file-name-base (org-remark-notes-file-name-function))
					     ".org"))))

(use-package org-cliplink)
(setq org-agenda-window-setup 'current-window)
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
(require 'ox-clip)
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
        visual-fill-column-center-text t
        visual-fill-column-fringes-outside-margins nil)
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

(defun fi/open-file-new-split ()
  "Open at mouse in other window"
  (interactive)
  (let ((org-link-frame-setup '((file . find-file-other-window))))
    (org-open-at-point)))

(global-set-key [C-down-mouse-1] 'fi/open-file-new-split)
(global-set-key [mouse-8] 'fi/switch-last-buffer)

(defun fi/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
(add-hook 'org-mode-hook #'fi/org-ispell)
(add-hook 'org-mode-hook 'flyspell-mode)

(defun fi/org-export-for-confluence ()
  "Export the current org buffer to markdown that can be copied in Conluence."
  (interactive)
  (org-md-export-as-markdown)
  (delete-matching-lines "^<a id=.*$"))

(fi/load-package-config ORG-MODULE-PATH "org-roam.el")

(provide 'fi-org)

;;; fi-org.el ends here
