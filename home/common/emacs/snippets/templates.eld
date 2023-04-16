;; ~/.config/emacs/templates

;; fundamental-mode

emacs-lisp-mode

(package-heading ";;; " (f-filename buffer-file-name) " --- " p n n ";;; Commentary:" n ";; " p n n ";;; Code:")
(package-footer ";;; " (f-filename buffer-file-name) " ends here")

nix-mode

(box "#" (make-string (length str) ?#) "#" n
     "#" (s str)                       "#" n
     "#" (make-string (length str) ?#) "#" n)

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
