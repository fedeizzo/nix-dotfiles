;;; init.el  --- init file

;;; Commentary:
;; Init file

;;; Code:


;;; Garbage Optimizations
;; Increase the GC threshold for faster startup
(setq gc-cons-threshold (* 2 50 1000 1000))


;;; Emacs lisp source/compiled preference
;; Prefer loading newest compiled .el file
(customize-set-variable 'load-prefer-newer t)


;;; Emacs startup profiling
;; Profile emacs startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))  ;; speed up startup time


;;; Emacs variable
;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;;; Native compilation
;; enable compilation of packages
(setq package-native-compile t)
;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)


;;; UI
(setq use-file-dialog nil)
(setq visible-bell nil)
(setq frame-inhibit-implied-resize t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mouse-color . "white") default-frame-alist)
(set-default-coding-systems 'utf-8)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)


;;; Package manager
;; disable the package manager ?
(setq package-quickstart nil)

;; load use package:
;;   - :init        -> before package load
;;   - :config      -> after package load
;;   - :commands    -> commands for defer package load
;;   - :bind        -> create key binds
;;   - :bind-keymap -> TODO understand usage
;;   - :hook        -> add hook in the form (hook . function/mode)
;;                     the suffix hook must be removed
;;                     if function specified as #', it is a reference
;;                     if function specified as ', it is evaluated
;;   - :custom      -> can be used to group setq execution
;;   - :custom-face -> can be used to group face customization
;;   - :if          -> conditional loading
;;   - :after       -> load package after another package
(eval-when-compile
  (require 'use-package))


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
(setq read-process-output-max (* 1024 1024))

;; disable warning for large files
(setq large-file-warning-threshold nil)


;;; NIX specific options
;; follows system link in the configuration
(setq vc-follow-symlinks t)
(setq ad-redefinition-action 'accept)
(setq create-lockfiles nil)

;; use esc to esc from command mode, find files, etc
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;;; General config
;; variable used to understand if I am using the macbook pro
(defconst ON-MACBOOK (eq system-type 'darwin))

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
(setq use-short-answers t)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Encoding
(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(prefer-coding-system       'utf-8)     ; Add utf-8 at the front for automatic detection.
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL
(set-language-environment "English")    ; Set up multilingual environment


;;; Load Path
;; add packages config
(add-to-list 'load-path (expand-file-name "~/.config/emacs/modules/utils"))
(add-to-list 'load-path (expand-file-name "~/.config/emacs/modules/ui"))
(add-to-list 'load-path (expand-file-name "~/.config/emacs/modules/keys"))
(add-to-list 'load-path (expand-file-name "~/.config/emacs/modules/completion"))
(add-to-list 'load-path (expand-file-name "~/.config/emacs/modules/org"))
(add-to-list 'load-path (expand-file-name "~/.config/emacs/modules/cluster-export"))
(add-to-list 'load-path (expand-file-name "~/.config/emacs/modules/checkers"))
(add-to-list 'load-path (expand-file-name "~/.config/emacs/modules/prog"))
(add-to-list 'load-path (expand-file-name "~/.config/emacs/modules/life-improvements"))

(require 'fi-utils)
(require 'fi-ui)
(require 'fi-keys)
(require 'fi-completion)
(require 'fi-org)
(require 'cluster-export)
(require 'fi-checkers)
(require 'fi-prog)
(require 'fi-life-improvements)


(nano-dark) ; the theme is loaded becasue in some cases some UI component does not inherit the right color

;;; init.el ends here
