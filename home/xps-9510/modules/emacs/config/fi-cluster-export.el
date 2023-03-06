(defconst cluster-export-buffer "*cluster-export-scratch*")

(defgroup cluster-export nil
  "Group for cluster export.")

(defcustom cluster-export-delete-regexes '("^#\\+title.*"
					   "^:PROPERTIES.*"
					   "^:ID.*"
					   "^:END.*"
					   "^:ROAM.*"
					   "^#\\+CREATED.*"
					   "^#\\+LAST_MODIFIED.*")
  "List of regexes to remove from a node buffer before export."
  :group 'cluster-export)

(defcustom cluster-export-header '("(concat \"#+title: \" (nth 0 (org-element-extract-heading-1)))"
				   "#+subtitle:"
				   "(concat \"#+UID: \" (nth 0 (org-element-extract-heading-1)))"
				   "(concat \"#+author: \"  user-full-name)"
				   "(concat \"#+date: \" (format-time-string \"%Y-%m-%d\"))"
				   "#+options: tex:dvipng")
  "List of regexes to remove from a node buffer before export."
  :group 'cluster-export)

(defcustom cluster-export-newpage-after-heading-1 t
  "Add \\clearpage command after every node"
  :type 'boolean
  :group 'cluster-export)

(defun org-element-extract-heading-level (headline)
  "Extract headings from org mode AST."
  (vector
   (org-element-property :raw-value headline)
   (org-element-property :level headline)))

(defun org-element-extract-heading-1 (&optional file)
  "Extract headings level 1 from org mode AST."
  (interactive)
  (setq buffer (current-buffer))
  (when file
    (setq-local buffer file)
    (when (not (bufferp file))
      (setq-local buffer (find-file-noselect file))))
  (with-current-buffer buffer
    (let* ((headings (org-element-map (org-element-parse-buffer) 'headline #'org-element-extract-heading-level))
	   (org-startup-with-latex-preview nil)
	   (result '()))
      (dolist (el headings)
	(if (= (aref el 1) 1)
	    (push (aref el 0) result)))
      result)))

(defun org-element-extract-images (filename)
  "Extract images from org mode AST."
  (with-current-buffer (find-file-noselect filename)
    (let* ((relative-images (org-element-map (org-element-parse-buffer) 'link
			      (lambda (link)
				(when (string= (org-element-property :type link) "file")
				  (org-element-property :path link)))))
	   (default-directory (file-name-directory filename))
	   (absolute-images (mapcar #'file-truename relative-images))
	   (org-startup-with-latex-preview nil))
      (list relative-images absolute-images))))

(defun cluster-export-eval-header (string)
  "Maybe evaluate elisp in a given STRING."
  (or (ignore-errors
	(eval (car (read-from-string (format "(progn %s)" string)))))
      string))

;; (defun cluster-export-get-all-relative-files ())

(defun cluster-export-get-all-org-roam-ids-current-buffer ()
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "id")
        (org-element-property :path link)))))

(defun cluster-export-get-org-level-from-list (regex-item)
  "Return the org heading level giving list in the buffer"
  (search-forward regex-item)
  (/ (current-indentation) 2))

(defun cluster-export-get-node-for-cluster-export ()
  "Return a list of pairs org node/indentation"
  (let ((line-pos (line-number-at-pos)))
    (beginning-of-buffer)
    (let* ((ids (cluster-export-get-all-org-roam-ids-current-buffer))
	   (roam-nodes (mapcar #'org-roam-node-from-id ids))
	   (indentations (mapcar #'cluster-export-get-org-level-from-list ids))
	   (filenames (mapcar #'org-roam-node-file roam-nodes)))
      (goto-line line-pos)
      (-zip roam-nodes indentations))
    ))

(defun cluster-export-delete-cluster-export-scratch-buffer ()
  "Delete the cluster-export-scratch buffer asking user's permission."
  (if (bufferp (get-buffer cluster-export-buffer))
      (kill-buffer cluster-export-buffer)))

;; credits to https://github.com/Vidianos-Giannitsis/zetteldesk.el/blob/c299afe798e9a77a175dab17a211f84ddb184461/zetteldesk.el#L364
(defun cluster-export-create-cluster-export-scratch-buffer ()
  "Create the cluster-export-scratch buffer and put it in `org-mode'."
  (cluster-export-delete-cluster-export-scratch-buffer)
  (let ((buffer (get-buffer-create cluster-export-buffer))
	(org-startup-with-latex-preview nil)
	(header-lines (mapcar
		       (lambda (el) (concat (cluster-export-eval-header el) "\n"))
		       cluster-export-header)))
    (with-current-buffer buffer
      (org-mode)
      (beginning-of-buffer)
      (mapc #'insert header-lines))))

(defun cluster-export-insert-node-into-cluster-export-buffer (node)
  (let* ((roam-node (car node))
	 (indentation-level (cdr node))
	 (filename (org-roam-node-file roam-node))
	 (org-startup-with-latex-preview nil)
	 (headings (reverse (org-element-extract-heading-1 filename)))
	 (images (org-element-extract-images filename)))
    (kill-buffer (find-file-noselect filename))
    (with-current-buffer cluster-export-buffer
      (goto-char (point-max))
      (newline)
      (insert-file-contents filename)
      (mapc #'kill-matching-lines cluster-export-delete-regexes)
      (dolist (heading headings)
	(dotimes (i indentation-level)
	  (let ((match-str (concat "ITEM={" heading "}")))
	    (org-map-entries (lambda () (org-demote-subtree)) match-str))))
      (when (and cluster-export-newpage-after-heading-1 (= indentation-level 0))
	(newline)
	(insert "\\clearpage\n")))
    images))

(defun cluster-export-rename-relative-to-absolute-images (relative-images absolute-images)
  (with-current-buffer cluster-export-buffer
    (dolist (image (-zip relative-images absolute-images))
      (beginning-of-buffer)
      (let ((relative (car image))
	    (absolute (cdr image)))
	(while (re-search-forward relative nil t)
	  (replace-match absolute))))))

(defun cluster-export ()
  "Export roam cluster following toc defined in current buffer."
  (interactive)
  (let ((nodes-indentations (cluster-export-get-node-for-cluster-export)))
    (cluster-export-create-cluster-export-scratch-buffer)
    (let ((relative-images '())
	  (absolute-images '()))
      (dolist (node nodes-indentations)
	(let ((images (cluster-export-insert-node-into-cluster-export-buffer node)))
	  (setq relative-images (append (first images) relative-images))
	  (setq absolute-images (append (second images) absolute-images))))
      (cluster-export-rename-relative-to-absolute-images
       (delete-dups relative-images)
       (delete-dups absolute-images)))
    (switch-to-buffer-other-window cluster-export-buffer)
    (beginning-of-buffer)))

(provide 'fi-cluster-export)
