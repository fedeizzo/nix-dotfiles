;;; svg-tag-mode.el --- TODO

;;; Commentary:
;; TODO

;;; Code:

(defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{3\\}")
(defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

(defun svg-progress-percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                    nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag (concat value "%")
                           nil :stroke 0 :margin 0)) :ascent 'center))

(defun svg-progress-count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
    (svg-image (svg-lib-concat
		(svg-lib-progress-bar (/ count total) nil
                                      :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		(svg-lib-tag value nil
                             :stroke 0 :margin 0)) :ascent 'center)))
(defun org-agenda-show-svg ()
  (let* ((case-fold-search nil)
         (keywords (mapcar #'svg-tag--build-keywords svg-tag--active-tags))
         (keyword (car keywords)))
    (while keyword
      (save-excursion
        (while (re-search-forward (nth 0 keyword) nil t)
          (overlay-put (make-overlay
                        (match-beginning 0) (match-end 0))
                       'display  (nth 3 (eval (nth 2 keyword)))) ))
      (pop keywords)
      (setq keyword (car keywords)))))

(use-package svg-tag-mode
  :custom
  (svg-tag-tags
   `(
     ;; Org tags
     (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
     (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

     ;; Task priority
     ("\\[#[A-Z]\\]" . ( (lambda (tag)
			   (svg-tag-make tag :face 'org-priority
                                         :beg 2 :end -1 :margin 0))))

     ;; Progress
     ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                         (svg-progress-percent (substring tag 1 -2)))))
     ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                       (svg-progress-count (substring tag 1 -1)))))

     ;; TODO / DONE
     ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
     ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


     ;; Citation of the form [cite:@Knuth:1984]
     ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                       (svg-tag-make tag
                                                     :inverse t
                                                     :beg 7 :end -1
                                                     :crop-right t))))
     ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag
							      :end -1
							      :crop-left t))))


     ;; Active date (with or without day name, with or without time)
     (,(format "\\(<%s>\\)" date-re) .
      ((lambda (tag)
         (svg-tag-make tag :beg 1 :end -1 :margin 0))))
     (,(format "\\(<%s \\)%s>" date-re day-time-re) .
      ((lambda (tag)
         (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
     (,(format "<%s \\(%s>\\)" date-re day-time-re) .
      ((lambda (tag)
         (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

     ;; Inactive date  (with or without day name, with or without time)
     (,(format "\\(\\[%s\\]\\)" date-re) .
      ((lambda (tag)
         (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
     (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
      ((lambda (tag)
         (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
     (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
      ((lambda (tag)
         (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date)))))))

;;; svg-tag-mode.el ends here
(add-hook 'org-agenda-finalize-hook #'org-agenda-show-svg)
