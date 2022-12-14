(defun fi/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . fi/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-return-follows-link t
        org-confirm-babel-evaluate nil
        org-catch-invisible-edits 'show
        org-src-window-setup 'current-window))
(use-package org-contrib)


(defun fi/dummy-org-download-annotate-function (link)
  ""
  "#+ATTR_ORG: :width 250px\n#+ATTR_LATEX: :width 250px :placement [H] \n#+CAPTION: \n#+NAME: \n"

  )

(use-package org-download
  :after org
  :config
  (setq org-downlaod-screenshot-method "grim -g \"$(slurp)\" - | swappy -f -")
  (setq org-download-annotate-function
        #'fi/dummy-org-download-annotate-function)
  (setq org-downlaod-image-dir "./img"))
(setq org-link-frame-setup '((file . find-file)))

(setq
 org-directory "~/org"
 fi/org-agenda-inbox-file (concat org-directory "/inbox.org")
 fi/org-agenda-work-file (concat org-directory "/work.org")
 fi/org-agenda-uni-file (concat org-directory "/uni.org")
 fi/org-agenda-personal-file (concat org-directory "/personal.org")
 fi/org-agenda-habits-file (concat org-directory "/habits.org")
 fi/org-agenda-files (list
                      fi/org-agenda-work-file
                      fi/org-agenda-uni-file
                      fi/org-agenda-personal-file))
(require 'org-habit)
(require 'org-protocol)
(use-package org-cliplink)
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "DOING(c)" "HOLD(h)" "|" "DONE(d)")))
(setq org-todo-keywords-for-agenda '((sequence "TODO(t)" "NEXT(n)" "DOING(c)" "HOLD(h)" "|" "DONE(d)")))
(setq org-todo-state-tags-triggers '((sequence "TODO(t)" "NEXT(n)" "DOING(c)" "HOLD(h)" "|" "DONE(d)")))
(setq org-agenda-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "DOING(c)" "HOLD(h)" "|" "DONE(d)")))
(setq-default org-enforce-todo-dependencies t)

(setq org-tag-alist
      '(("@work" . ?w)
        ("@uni" . ?u)
        ("@home" . ?h)))

(setq
 org-agenda-files (list org-directory)
 org-agenda-breadcrumbs-separator " ❱ "
 org-agenda-block-separator "──────────"
 org-agenda-include-inactive-timestamps t
 org-agenda-log-mode-items '(closed clock state)
 org-agenda-start-with-log-mode '(closed clock state)
 org-agenda-tags-column 0
 org-columns-default-format "%60ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock) %TAGS"
 )
(setq org-agenda-custom-commands
      '(
        (" " "Agenda"
         (
          (agenda ""
                  ((org-agenda-overriding-header  " This week")))
          (todo "TODO"
                ((org-agenda-overriding-header " To process / Inbox")
                 (org-agenda-prefix-format "%b")
                 (org-super-agenda-groups nil)
                 (org-agenda-files (list fi/org-agenda-inbox-file))))
          (todo "NEXT"
                ((org-agenda-overriding-header " NEXT")
                 (org-agenda-prefix-format "")
                 (org-super-agenda-groups '((:auto-category t)))))
          (todo "DOING"
                ((org-agenda-overriding-header " DOING")
                 (org-agenda-prefix-format "")
                 (org-super-agenda-groups '((:auto-category t)))))
          (todo "HOLD"
                ((org-agenda-overriding-header " HOLD")
                 (org-agenda-prefix-format "")
                 (org-super-agenda-groups '((:auto-category t)))))
          (todo "TODO"
                ((org-agenda-overriding-header " TODO")
                 (org-agenda-prefix-format "")
                 (org-agenda-files fi/org-agenda-files)
                 (org-super-agenda-groups '((
                                             :auto-category t
                                             :auto-priority t
                                             )))))
          )
         )
        ))

(defun fi/refile-task ()
  (interactive)
  (setq org-refile-targets '((fi/org-agenda-files :maxlevel . 1)))
  (org-agenda-set-tags)
  (org-agenda-priority)
  (org-agenda-set-effort)
  (org-agenda-refile)
  (setq org-refile-targets nil)
  (org-agenda-redo-all))

(setq org-agenda-window-setup 'current-window)

(pretty-hydra-define agenda-hydra-main (:color blue :title "Agenda" :quit-key "q")
  ("Task"
   (("r" fi/refile-task "refile")
    ("t" org-agenda-todo "todo status")
    ("i" org-agenda-clock-in "clock in")
    ("o" org-agenda-clock-out "clock out")
    ))
  )
(defun fi/switch-to-agenda ()
  (interactive)
  (org-agenda nil " "))

(fi/leader "a" #'fi/switch-to-agenda)
(define-key org-agenda-mode-map (kbd "SPC") 'agenda-hydra-main/body)

(use-package org-super-agenda
  :config
  (add-hook 'org-agenda-mode-hook #'org-super-agenda-mode))

(setq org-default-notes-file fi/org-agenda-inbox-file)
(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar (lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))
(setq org-capture-templates
      '(
        ("t" "To-do task to process" entry (file+headline fi/org-agenda-inbox-file "Task") "* TODO%?\n" :empty-lines 1)
        ("r" "Thing to read" entry (file+headline fi/org-agenda-inbox-file "Manual") "* TODO %(org-cliplink-capture)\n" :empty-lines 1 :immediate-finish t)
        ("p" "Protocol text" entry (file+headline fi/org-agenda-inbox-file "Protocol") "* TODO %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?" :empty-lines 1 :immediate-finish t)
        ("L" "Protocol link" entry (file+headline fi/org-agenda-inbox-file "Protocol") "* TODO [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n#+CREATED: [%<%Y-%m-%d %a %H:%M:%S>]\n" :empty-lines 1 :immediate-finish t)
        ))
(global-set-key (kbd "C-c c") #'org-capture)

(use-package async)

(require 'org-tempo)
(require 'ob-python)
(require 'ob-hledger)
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("li" . "src lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

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

(use-package org-modern
  :config
  (setq
   org-modern-block t
   org-pretty-entities nil
   org-modern-table nil
   org-modern-hide-stars nil))
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
(with-eval-after-load 'org-faces
  ;; Make sure org-indent face is available
  (require 'org-indent)
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (defun fi/font-config-org (frame)
    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font "JetBrains Mono" :weight 'regular :height (cdr face)))
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))
  (remove-hook 'after-make-frame-functions #'fi/font-config-org)
  (add-hook 'after-make-frame-functions #'fi/font-config-org))

(setq org-startup-with-latex-preview t)
(setq org-image-actual-width nil)

(defun fi/get-sway-inkscape-location ()
  (json-parse-string
   (shell-command-to-string "swaymsg -t get_tree | jq '.. | select(.type?) | select(.app_id==\"org.inkscape.Inkscape\") | .rect'")))

(defun fi/get-windows-location ()
  (json-parse-string
   (shell-command-to-string "swaymsg -t get_tree | jq ' .rect'")))

(defun fi/set-tablet-location (x y width height)
  (if (< y 0)
      (shell-command (format "swaymsg input 1386:890:Wacom_One_by_Wacom_S_Pen map_to_region %d %d %d %d" x 0 width height) nil nil)
    (shell-command (format "swaymsg input 1386:890:Wacom_One_by_Wacom_S_Pen map_to_region %d %d %d %d" x y width height) nil nil)
    ))

(defun fi/set-inkscape ()
  (let*
      ((sway-tree (fi/get-sway-inkscape-location))
       (x (gethash "x" sway-tree))
       (y (gethash "y" sway-tree))
       (width (gethash "width" sway-tree))
       (height (gethash "height" sway-tree)))
    (fi/set-tablet-location x y width height)
    ))

(defun fi/reset-tablet-to-windows ()
  (let*
      ((sway-tree (fi/get-windows-location))
       (x (gethash "x" sway-tree))
       (y (gethash "y" sway-tree))
       (width (gethash "width" sway-tree))
       (height (gethash "height" sway-tree)))
    (fi/set-tablet-location x y width height)
    ))

(defun fi/reset-images-inkscape-diagrame-mode (process event)
  (org-display-inline-images)
  (org-display-inline-images)
  (fi/reset-tablet-to-windows))

(defun fi/org-roam-inkscape-diagram ()
  "Create or edit an svg file with inkscape and add link to current org document"
  (interactive)
  (let* '(filename (format "./figures/%s" (completing-read "SVG file: "
                                                           (directory-files "./figures" nil ".*svg$" nil nil))))
    (when (not (file-exists-p filename))
      (copy-file "/home/fedeizzo/zettelkasten/template.svg" filename)
      (insert (format "
  ,#+ATTR_ORG: :width 450px
  ,#+ATTR_LATEX: :width 450px :placement [H]
  ,#+CAPTION:
  ,#+NAME:
  [[file:%s]]
  " filename)))
    (setq proc (start-process "ink" nil "inkscape" (format "%s" (expand-file-name filename))))
    (sleep-for 0.5)
    (fi/set-inkscape)
    (set-process-sentinel proc 'fi/reset-images-inkscape-diagrame-mode)
    ))
(use-package tex-site
  :config
  (setq TeX-parse-self t
        TeX-auto-save t))
(with-eval-after-load 'ox-latex
  (setq org-latex-classes nil)
  (add-to-list 'org-latex-classes
               '("personal"
                 "\\documentclass[a4paper,11pt,notitlepage,margin=2.5cm]{article}
                      \\usepackage[utf8]{inputenc}
                      \\usepackage[T1]{fontenc}
                      \\usepackage{textcomp}
                      \\usepackage{url}
                      \\usepackage{graphicx}
                      \\usepackage{hyperref}
                      \\usepackage{float}
                      \\usepackage{parskip}
                      \\usepackage{xcolor}
                      \\usepackage{amsmath, amsfonts, mathtools, amsthm, amssymb}
                      \\usepackage{enumitem}
                      \\setlist[itemize]{noitemsep}
                      \\usepackage{geometry}
                      \\geometry{
                          a4paper,
                          total={170mm,257mm},
                          left=20mm,
                          top=20mm,
                      }
                      % for svg images from tex files
                      \\usepackage{import}
                      \\usepackage{xifthen}
                      \\usepackage{pdfpages}
                      \\usepackage{transparent}
                      \\newcommand{\\incfig}[1]{%
                          \\def\\svgwidth{\\columnwidth}
                          \\import{.}{#1.pdf_tex}
                      }

                      % Polar Night
                      \\definecolor{NordDarkBlack}{HTML}{2E3440}     % nord0
                      \\definecolor{NordBlack}{HTML}{3B4252}         % nord1
                      \\definecolor{NordMediumBlack}{HTML}{434C5e}   % nord2
                      \\definecolor{NordBrightBlack}{HTML}{4C566A}   % nord3
                      % Snow Storm
                      \\definecolor{NordWhite}{HTML}{D8DEE9}         % nord4
                      \\definecolor{NordBrighterWhite}{HTML}{E5E9F0}         % nord5
                      \\definecolor{NordBrightestWhite}{HTML}{ECEFF4}   % nord6
                      % Frost
                      \\definecolor{NordCyan}{HTML}{8FBCBB}          % nord7
                      \\definecolor{NordBrightCyan}{HTML}{88C0D0}    % nord8
                      \\definecolor{NordBlue}{HTML}{81A1C1}          % nord9
                      \\definecolor{NordBrightBlue}{HTML}{5E81AC}    % nord10
                      % Aurora
                      \\definecolor{NordRed}{HTML}{BF616A}           % nord11
                      \\definecolor{NordOrange}{HTML}{D08770}        % nord12
                      \\definecolor{NordYellow}{HTML}{EBCB8B}        % nord13
                      \\definecolor{NordGreen}{HTML}{A3BE8C}         % nord14
                      \\definecolor{NordMagenta}{HTML}{B48EAD}       % nord15

                      \\hypersetup{
                          colorlinks=true,
                          linkcolor=black,
                          filecolor=NordBrightBlack,
                          urlcolor=NordBrightBlack,
                          citecolor=NordBrightBlack,
                      }
                      \\urlstyle{same}
                      \\renewcommand\\contentsname{
                        ~\\hfill {\\LARGE Table of contents}\\\\
                        \\rule{\\textwidth}{0.4pt}
                      }
                      "
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-default-class "personal"))

(setq org-format-latex-options '(
                                 :foreground default
                                 :background default
                                 :scale 1.0
                                 :html-foreground "Black"
                                 :html-background "Transparent"
                                 :html-scale 1.0
                                 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

(setq org-latex-title-command "
      \\begin{titlepage}
              \\raggedleft
              \\vspace*{\\baselineskip}
              {\\Large %a}\\\\
              \\vspace*{0.167\\textheight}
              \\textbf{\\LARGE Personal notes of}\\\\[\\baselineskip]
              {{\\color{NordMediumBlack}{\\Huge %t}}\\\\[\\baselineskip]}
              {\\Large \\textit{%s}}
              \\vfill
              {\\large $\\mathcal{FI}$}
              \\vspace*{3\\baselineskip}
      \\end{titlepage}
      ")
(setq org-latex-toc-command "\\tableofcontents \\clearpage")
(setq org-export-headline-levels 5)
(setq org-startup-with-latex-preview t)
(use-package org-fragtog
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))
(add-to-list 'org-structure-template-alist '("al" . "src latex\n \\begin{align*}\n\\end{align*}\n"))
(setq org-latex-pdf-process
      (let
          ((cmd (concat "pdflatex -shell-escape -interaction nonstopmode"
                        " --synctex=1"
                        " -output-directory %o %f")))
        (list cmd
              "cd %o; if test -r %b.idx; then makeindex %b.idx; fi"
              "cd %o; bibtex %b"
              cmd
              cmd)))
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
    (setq org-latex-pdf-process '("latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))
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
(use-package org-roam
  :after org)
(require 'org-roam)
(setq org-roam-directory (file-truename "~/zettelkasten"))
(org-roam-db-autosync-mode)
(setq org-roam-mode-section-functions
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section))
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.33)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))

(defun fi/org-roam-ui-browser-function (url)
  (interactive)
  (shell-command "hyprctl keyword windowrulev2 'float, class:firefox'")
  (shell-command "hyprctl keyword windowrulev2 'size 75% 75%, class:firefox'")
  (shell-command "hyprctl keyword windowrulev2 'center, class:firefox'")
  (shell-command (concat "sleep 0.5 && firefox --new-window " url))
  (shell-command "hyprctl keyword windowrulev2 'unset, class:firefox'"))

(use-package org-roam-ui
  :commands org-roam-ui-open
  :config
  (setq
   org-roam-ui-open-on-start nil
   org-roam-ui-follow t
   org-roam-ui-browser-function 'fi/org-roam-ui-browser-function))

(setq org-id-track-globally t)
;; I encountered the following message when attempting
;; to export data:
;;
;; "org-export-data: Unable to resolve link: FILE-ID"
(defun fi/force-org-rebuild-cache ()
  "Rebuild the `org-mode' and `org-roam' cache."
  (interactive)
  (org-id-update-id-locations)
  ;; Note: you may need `org-roam-db-clear-all'
  ;; followed by `org-roam-db-sync'
  (org-roam-db-sync)
  (org-roam-update-org-id-locations))
(defun fi/org-roam-extract-topic ()
  (let* (
         ;; (cwd (file-name-directory (other-buffer)))
         (cwd (file-name-directory (buffer-file-name (nth 1 (buffer-list)))))
         (current-topic (string-replace (concat org-roam-directory "/") "" cwd)))
    (if (string= current-topic "")
        ""
      current-topic
      )))
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "%(fi/org-roam-extract-topic)${slug}.org" "#+title: ${title}\n#+CREATED: [%<%Y-%m-%d %a %H:%M:%S>]\n#+LAST_MODIFIED: [%<%Y-%m-%d %a %H:%M:%S>]")
         :unnarrowed t)))

(defun fi/toggle-org-roam-ui-follow ()
  "Toggle roam ui follow mode"
  (interactive)
  (if (bound-and-true-p org-roam-ui-follow-mode)
      (org-roam-ui-follow-mode -1)
    (org-roam-ui-follow-mode t)))

(pretty-hydra-define org-roam-hydra-main (:color blue :title "Org roam" :quit-key "q")
  ("Node"
   (("i" org-roam-node-insert "inesert node")
    ("f" org-roam-node-find "find node")
    ("h" org-id-get-create "add id current node"))
   "Clipboard"
   (("c" org-download-clipboard "paste clipboard"))
   "Inkscape"
   (("s" fi/org-roam-inkscape-diagram "open/edit svg file"))
   "Custom functions"
   (("r" fi/rename-images-in-file-with-caption "sync filename with caption")
    ("e" fi/zetteldesk-insert-all-nodes-contents-current-buffer-list "export roam cluster"))
   "Roam UI"
   (("l" org-roam-ui-node-local "open current node")
    ("a" org-roam-ui-add-to-local-graph "add current node")
    ("d" org-roam-ui-remove-from-local-graph "delete current node")
    ("u" fi/toggle-org-roam-ui-follow "toggle follow mode"))))

(fi/leader "n" 'org-roam-hydra-main/body)
(use-package zetteldesk
  :after org-roam
  :load-path "~/.config/emacs/zetteldesk.el"
  :config
  (zetteldesk-mode))
(require 'zetteldesk)
(defun fi/get-all-org-roam-ids-current-buffer ()
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "id")
        (org-element-property :path link)))))

(defun fi/get-all-relative-files-current-buffer ()
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "file")
        (org-element-property :path link)))))

(defun fi/get-org-level-from-list (regex-item)
  "Return the org heading level giving list in the buffer"
  (search-forward regex-item)
  (/ (current-indentation) 2))


(defun fi/demote-org-roam-node (level text)
  "Demote an org tree given its level and regex"
  (let ((match-str (concat "LEVEL=" (number-to-string level) "+ITEM={" text "}")))
    (org-map-entries (lambda () (org-demote-subtree)) match-str))
  )

(defun fi/delete-properties-drawer ()
  "Delete properties drawers and its content"
  (kill-matching-lines "^#\\+title.*")
  (kill-matching-lines "^:PROPERTIES.*")
  (kill-matching-lines "^:ID.*")
  (kill-matching-lines "^:END.*")
  (kill-matching-lines "^:ROAM.*"))

(defun fi/zetteldesk-insert-all-nodes-contents-current-buffer-list ()
  (interactive)
  ;; (fi/force-org-rebuild-cache)
  (beginning-of-buffer)
  (setq new-headings '())
  (setq absolute-file-links '())
  (setq org-startup-with-latex-preview nil)
  (dolist (id (fi/get-all-org-roam-ids-current-buffer))
    (let* ((node (org-roam-node-from-id id))
           (filename (org-roam-node-file node))
           (org-level (fi/get-org-level-from-list id))
           (node-buffer (find-file-noselect filename))
           (location (zetteldesk-insert-location)))
      (with-current-buffer node-buffer
        (setq heading-texts (org-map-entries (lambda () (fifth (org-heading-components))) "LEVEL=1"))
        (dolist (heading-text heading-texts)
          (when (not (= org-level 0))
            (push (list org-level heading-text) new-headings)
            ))
        (dolist (link (fi/get-all-relative-files-current-buffer))
          (push (list link (file-truename link)) absolute-file-links))
        )
      (kill-buffer node-buffer)
      (with-current-buffer location
        (goto-char (point-max))
        (newline)
        (insert-file-contents filename)
        (fi/delete-properties-drawer))
      ))
  (setq new-headings (reverse new-headings))
  (let ((location (zetteldesk-insert-location)))
    (with-current-buffer location
      (org-mode)
      (beginning-of-buffer)
      (insert "
  ,#+TITLE:
  ,#+SUBTITLE:
  ,#+UID:
  ,#+AUTHOR:
  ,#+DATE:
  ,#+OPTIONS: tex:dvipng")
      (dolist (new-heading new-headings)
        (dotimes (level (first new-heading))
          (fi/demote-org-roam-node (+ level 1) (second new-heading))))
      (dolist (link-pair absolute-file-links)
        (let ((relative-link (first link-pair))
              (absolute-link (second link-pair)))
          (beginning-of-buffer)
          (while (re-search-forward relative-link nil t)
            (replace-match absolute-link))
          )
        )
      ))
  (switch-to-buffer-other-window "*zetteldesk-scratch*")
  (setq org-startup-with-latex-preview t)
  )
(defun fi/get-all-images (&optional element)
  (org-element-map (or element (org-element-parse-buffer)) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "file")
        (org-element-property :path link)))))

(defun fi/get-all-paragraphs ()
  (org-element-map (org-element-parse-buffer) 'paragraph
    (lambda (paragraph)
      paragraph)))

(defun fi/extract-caption-from-paragraph (paragraph)
  (if-let (caption (org-element-property :caption paragraph))
      (substring-no-properties (first (first (first caption))))
    ))

(defun fi/caption-as-filename (caption)
  (concat (replace-regexp-in-string "[\(\)\.]" ""
                                    (replace-regexp-in-string " " "_" caption)) ".png")
  )

(defun fi/obtain-filename-pairs ()
  (let ((filename-pairs '()))
    (dolist (paragraph (fi/get-all-paragraphs))
      (let ((caption (fi/extract-caption-from-paragraph paragraph)))
        (when caption
          (let* ((image-path (first (fi/get-all-images paragraph)))
                 (image-dir (file-name-directory image-path))
                 (new-image-name (fi/caption-as-filename caption))
                 (new-image-path (concat image-dir new-image-name)))
            (push (list image-path new-image-path) filename-pairs)))))
    filename-pairs))

(defun fi/rename-images-in-file-with-caption ()
  (interactive)
  (dolist (filenames-pair (fi/obtain-filename-pairs))
    (let ((old-path (first filenames-pair))
          (new-path (second filenames-pair)))
      (when (not (string= old-path new-path))
        (rename-file old-path new-path)
        (beginning-of-buffer)
        (while (re-search-forward old-path nil t)
          (replace-match new-path)
          (org-toggle-inline-images)
          (org-toggle-inline-images)))
      )))
(pretty-hydra-define zetteldesk-add-hydra (:color blue :title "Add to Zetteldesk" :quit-key "q")
  ("Org-Roam"
   (("n" zetteldesk-add-node-to-desktop "Add Node")
    ("b" zetteldesk-add-backlinks-to-desktop "Add Node + All its backlinks"))
   "Other"
   (("a" zetteldesk-add-to-desktop "Add Buffer"))))

(pretty-hydra-define zetteldesk-remove-hydra (:color blue :title "Remove from Zetteldesk" :quit-key "q")
  ("Org-Roam"
   (("n" zetteldesk-remove-node-from-desktop "Remove Node")
    ("b" zetteldesk-remove-backlinks-from-desktop "Remove Node + All its backlinks"))
   "Other"
   (("r" zetteldesk-remove-from-desktop "Remove Buffer"))))

(pretty-hydra-define zetteldesk-insert-hydra (:color blue :title "Insert from the Zetteldesk" :quit-key "q")
  ("Org-Roam"
   (("n" zetteldesk-insert-node-contents-without-link "Node Contents in *zetteldesk-scratch")
    ("a" fi/zetteldesk-insert-all-nodes-contents-without-link  "All nodes Contents in *zetteldesk-scratch*")
    ("B" fi/zetteldesk-insert-all-nodes-contents-current-buffer  "All nodes Contents in current buffer in *zetteldesk-scratch*"))
   "Supplementary Material to *zetteldesk-scratch*"
   (("p" zetteldesk-insert-link-to-pdf "Link to PDF"))))

(pretty-hydra-define zetteldesk-main-hydra (:color blue :title "Zetteldesk Hydra" :quit-key "q")
  ("Filter Functions"
   (("n" zetteldesk-node-find "Find Zetteldesk Node"))

   "Add/Remove Hydras"
   (("r" zetteldesk-remove-hydra/body "Run the Removing Hydra")
    ("a" zetteldesk-add-hydra/body "Run the Adding Hydra"))

   "Inserting Things and *zetteldesk-scratch*"
   (("s" zetteldesk-switch-to-scratch-buffer "Switch to *zetteldesk-scratch*")
    ("i" zetteldesk-insert-hydra/body "Run the Insert Hydra"))))

(fi/leader "u" 'zetteldesk-main-hydra/body)
;; line number
(column-number-mode)
(global-display-line-numbers-mode t)
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

(defun fi/zetteldesk-add-current-buffer-to-desktop ()
  "Add current buffer to zetteldek desktop"
  (interactive)
  (zetteldesk-add-to-desktop (current-buffer)))

(defun fi/open-file-new-split ()
  "Open at mouse in other window"
  (interactive)
  (let ((org-link-frame-setup '((file . find-file-other-window))))
    (org-open-at-point)))

(global-set-key [C-down-mouse-1] 'fi/open-file-new-split)
(global-set-key [mouse-8] 'fi/switch-last-buffer)
(global-set-key [mouse-9] 'fi/zetteldesk-add-current-buffer-to-desktop)

(provide 'fi-org)
