(use-package super-save
  :config
  (setq super-save-remote-files nil)
  (setq super-save-exclude '("*unsent mail*"))
  (setq auto-save-default nil)
  (super-save-mode 1))
