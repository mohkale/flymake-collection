;;; flymake-rest-yamllint.el --- Yamllint diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest-define)

(eval-when-compile
  (require 'flymake-rest-rx))

(flymake-rest-define flymake-rest-yamllint
  "A YAML syntax checker using YAMLLint.
See URL `https://github.com/adrienverge/yamllint'."
  :title "yamllint"
  :pre-let ((yamllint-exec (executable-find "yamllint")))
  :pre-check (unless yamllint-exec
               (error "Cannot find yamllint executable"))
  :write-type 'pipe
  :command (list yamllint-exec "-f" "parsable" "-")
  :error-parser
  (flymake-rest-parse-rx
   ((error   bol "stdin:" line ":" column ": " "[error] "   (message) eol)
    (warning bol "stdin:" line ":" column ": " "[warning] " (message) eol))))

(provide 'flymake-rest-yamllint)
