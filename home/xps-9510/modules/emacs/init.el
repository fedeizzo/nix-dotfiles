(setq user-init-file "~/.config/emacs/init.el")
;; (setq warning-minimum-level :error)
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 2 50 1000 1000))
(setq read-process-output-max (* 1024 1024))
;; enable compilation of packages
(setq package-native-compile t)


;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))  ;; speed up startup time
(setq frame-inhibit-implied-resize t)

(setq package-quickstart nil)
;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))
;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)
;;; Runtime optimizations
;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other
;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb

(setq large-file-warning-threshold nil)
(setq vc-follow-symlinks t)
(setq ad-redefinition-action 'accept)
(setq create-lockfiles nil)

;; use esc to esc from command mode, find files, etc
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; global-set-key sets the keymap for all modes
;; define-key assicoates the keymap only to a specific mode (emacs-lisp-mode-map variable to add maps)
(global-set-key (kbd "M-o") 'bufler-switch-buffer)

(setq vterm-kill-buffer-on-exit t)
(add-to-list 'load-path (expand-file-name "~/.config/emacs/config"))
(eval-when-compile
  (require 'use-package))

(require 'fi-ui)
(require 'fi-keys)
(require 'fi-minibuffer)
(require 'fi-navigation)
(require 'fi-benchmark)
(require 'fi-buffer)
(require 'fi-editing)
(require 'fi-org)
(require 'fi-cluster-export)
(require 'fi-mail)
(require 'fi-money)
;; (require 'fi-eaf)
