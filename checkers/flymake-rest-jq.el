;;; flymake-rest-jq.el --- jq diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest)

(eval-when-compile
  (require 'flymake-rest-define)
  (require 'flymake-rest-parse-rx))

;;;###autoload (autoload 'flymake-rest-jq "flymake-rest-jq")
(flymake-rest-define flymake-rest-jq
  "JSON checker using the jq tool.

This checker accepts multiple consecutive JSON values in a
single input, which is useful for jsonlines data.

See URL `https://stedolan.github.io/jq/'."
  :title "jq"
  :pre-let ((jq-exec (executable-find "jq")))
  :pre-check (unless jq-exec
               (error "Cannot find jq executable"))
  :write-type 'pipe
  :command (list jq-exec "." "-" null-device)
  :error-parser
  (flymake-rest-parse-rx
   ((error bol "parse error: " (message) " at line " line ", column " column eol))))

(provide 'flymake-rest-jq)

