;;; string-inflection.el --- string inflection

;;; Commentary:
;; util functions

;;; Code:

(pretty-hydra-define fi/string-inflection (:color blue :title "String transformation" :quit-key "q")
  ("CamelCase"
   (("cl" string-inflection-lower-camelcase "fooBar")
    ("cu" string-inflection-camelcase "FooBar"))
   "snake_case"
   (("sl" string-inflection-underscore "foo bar")
    ("su" string-inflection-capital-underscore "Foo Bar"))
   "kebab-case"
   (("k" string-inflection-kebab-case "foo-bar"))
   ))

(defun fi/string-transformation ()
  "Call the hydra for string inflection."
  (interactive)
  (fi/string-inflection/body))


;;; string-inflection.el ends here
