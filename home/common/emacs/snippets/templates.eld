;; ~/.config/emacs/templates

;; fundamental-mode

emacs-lisp-mode

(package-heading ";;; " (f-filename buffer-file-name) " --- " p n n ";;; Commentary:" n ";; " p n n ";;; Code:")
(package-footer ";;; " (f-filename buffer-file-name) " ends here")

python-mode

(pdb "import pdb;pdb.set_trace()")

nix-mode

(box "#" (make-string (length str) ?#) "#" n
     "#" (s str)                       "#" n
     "#" (make-string (length str) ?#) "#" n)

go-mode

(tsr "t.Run(\"" p "\", func(t *testing.T) {" n> p n> "})")
(rer "require.Error(t, " p")")
(req "require.Equal(t, " p", " p")")
(ril "require.Nil(t, " p")")

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
