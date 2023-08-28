(use-package magit
  :custom
  (magit-refresh-status-buffer nil "Performance optimization"))

(define-key magit-status-mode-map (kbd "x") 'magit-discard)

;; (use-package magit-todos
;;   :config
;;   (magit-todos-mode t))

(pretty-hydra-define magit-not-standard-commands
  (:title (fi/hydra-title-factory-faicon "git" "Magit not standard commands") :quit-key "q" :exit t)
  (" From other branch"
   (("bc" magit-file-checkout "Checkout file")
    ("bf" magit-find-file "Visist file")
    )
   " Diff"
   (("dh" git-gutter:popup-hunk "Popup hunk"))
   ))

;; (fi/leader "g" 'magit-not-standard-commands/body)

(pretty-hydra-define fi/rebase-helper (:color blue :title "Rebase helper" :quit-key "q" :exit nil)
  ("Movement"
   (("n" smerge-next "next conflict")
    ("p" smerge-previous "previous conflict"))
   "Keep"
   (("u" smerge-keep-upper "upper")
    ("l" smerge-keep-lower "lower")
    ("b" smerge-keep-both "both"))))

;; (fi/leader "m" #'fi/rebase-helper/body)
