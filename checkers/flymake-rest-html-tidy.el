;;; flymake-rest-html-tidy.el --- Tidy-HTML5 diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest)

(eval-when-compile
  (require 'flymake-rest-define)
  (require 'flymake-rest-parse-rx))

;;;###autoload (autoload 'flymake-rest-html-tidy "flymake-rest-html-tidy")
(flymake-rest-define flymake-rest-html-tidy
  "A HTML syntax and style checker using Tidy.

See URL `https://github.com/htacg/tidy-html5'."
  :title "tidy"
  :pre-let ((tidy-exec (executable-find "tidy")))
  :pre-check (unless tidy-exec
               (error "Cannot find tidy executable"))
  :write-type 'pipe
  :command `(,tidy-exec "-lang" "en" "-e" "-q")
  :error-parser
  (flymake-rest-parse-rx
   ((error   bol "line " line " column " column " - Error: "   (message) eol)
    (warning bol "line " line " column " column " - Warning: " (message) eol))))

(provide 'flymake-rest-html-tidy)
