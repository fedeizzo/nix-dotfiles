(use-package amx
  :config
  (amx-mode 1))

(use-package ivy
  :diminish ; diminish prevent to show the mode in the mode list in the mode line
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-n" . ivy-net-line)
         ("C-p" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ("C-p" . ivy-previous-line)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-p" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
;; better minibuffer command
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)
         )
  :config
  (setq ivy-initial-inputs-alist nil))
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; better help message
(use-package helpful
  :custom
  (counsel-describe-function #'helpful-callable)
  (counsel-describe-function #'helpful-variable)
  :bind
  ([remap describle-function] . counsel-describe-function)
  ([remap describle-command] . helpful-command)
  ([remap describle-variable] . counsel-describe-variable)
  ([remap describle-key] . helpful-key))

(provide 'fi-minibuffer)
