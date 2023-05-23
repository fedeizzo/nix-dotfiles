(use-package git-gutter-fringe
  :config
  (git-gutter-mode))

(fi/leader
  "g" 'git-gutter:popup-hunk)
