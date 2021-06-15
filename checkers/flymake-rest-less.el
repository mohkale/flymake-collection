;;; flymake-rest-less.el --- Less diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest-define)

(eval-when-compile
  (require 'flymake-rest-rx))

(flymake-rest-define flymake-rest-less
  "A LESS syntax checker using lessc.

Requires lessc 1.4 or newer.

See URL `http://lesscss.org'."
  :title "lessc"
  :pre-let ((lessc-exec (executable-find "lessc")))
  :pre-check (unless lessc-exec
               (error "Cannot find lessc executable"))
  :write-type 'pipe
  :command (list lessc-exec  "--lint" "--no-color" "-")
  :error-parser
  (flymake-rest-parse-rx
   ((error bol (+ not-newline) ": " (message) " in - on line " line ", column " column ":" eol))))

(provide 'flymake-rest-less)

