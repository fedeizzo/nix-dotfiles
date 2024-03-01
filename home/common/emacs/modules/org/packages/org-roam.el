;;; org-roam.el --- org-roam.el configuration

;;; Commentary:
;; Org roam configuration

;;; Code:
(use-package org-roam
  :after org
  :custom
  (org-roam-directory (file-truename "~/zettelkasten"))
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-update-on-save t)
  (org-roam-file-exclude-regexp '("no_index"))
  (org-roam-dailies-directory "00-agenda/daily/")
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-dailies-capture-templates
	'(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n* Summary\n\n* New todos\n\n* Work\n** Todos\n\n** Meeting notes\n\n** People\n*** Me\n")))))

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
   org-roam-ui-browser-function (if ON-MACBOOK
				    'browse-url
				  'fi/org-roam-ui-browser-function)))

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
   "Dailies"
   (("n" org-roam-dailies-capture-date "new dailies")
    ("v" org-roam-dailies-goto-date "view dailies"))
   "Custom functions"
   (("r" fi/rename-images-in-file-with-caption "sync filename with caption")
    ;; ("e" fi/zetteldesk-insert-all-nodes-contents-current-buffer-list "export roam cluster")
    )
   "Roam UI"
   (("l" org-roam-ui-node-local "open current node")
    ("a" org-roam-ui-add-to-local-graph "add current node")
    ("d" org-roam-ui-remove-from-local-graph "delete current node")
    ("u" fi/toggle-org-roam-ui-follow "toggle follow mode"))))

;; (fi/leader "n" 'org-roam-hydra-main/body)
;; (use-package zetteldesk
;;   :after org-roam
;;   :load-path "~/.config/emacs/zetteldesk.el"
;;   :config
;;   (zetteldesk-mode))
;; (require 'zetteldesk)

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

;;; org-roam.el ends here
