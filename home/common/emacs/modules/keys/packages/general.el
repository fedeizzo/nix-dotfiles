(use-package general
  :config
  (general-evil-setup t)
  (general-override-mode 1)
  (general-create-definer fi/leader
    :states 'normal
    :keymaps '(override)
    :prefix "C-P")
  (fi/leader
    "s" 'save-buffer
    ;; origami
    "zc" 'origami-close-node
    "zC" 'origami-close-all-nodes
    "zo" 'origami-open-node
    "zO" 'origami-open-all-nodes
    "zr" 'origami-close-node-recursively
    "zR" 'origami-open-node-recursively
    ;; neotree
    "t" 'treemacs))
