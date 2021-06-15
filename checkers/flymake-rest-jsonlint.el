;;; flymake-rest-jsonlint.el --- JSONLint diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest)

(eval-when-compile
  (require 'flymake-rest-define)
  (require 'flymake-rest-parse-rx))

;;;###autoload (autoload 'flymake-rest-jsonlint "flymake-rest-jsonlint")
(flymake-rest-define flymake-rest-jsonlint
  "A JSON syntax and style checker using jsonlint.

See URL `https://github.com/zaach/jsonlint'."
  :title "jsonlint"
  :pre-let ((jsonlint-exec (executable-find "jsonlint")))
  :pre-check (unless jsonlint-exec
               (error "Cannot find jsonlint executable"))
  :write-type 'file
  :command (list jsonlint-exec "-c" "-q" fmqd-temp-file)
  :error-parser
  (flymake-rest-parse-rx
   ((error bol (file-name) ": line " line ", col " column ", " (message) eol))))

(provide 'flymake-rest-jsonlint)

