;;; flymake-rest-awk-gawk.el --- awk/gawk diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest-define)

(eval-when-compile
  (require 'flymake-rest-parse-rx))

;;;###autoload (autoload 'flymake-rest-awk-gawk "flymake-rest-awk-gawk")
(flymake-rest-define flymake-rest-awk-gawk
  "GNU awk's built-in --lint checker."
  :title "gawk-awk"
  :pre-let ((gawk-exec (executable-find "gawk")))
  :pre-check (unless gawk-exec
               (error "Cannot find gawk executable"))
  :write-type 'pipe
  :command (list gawk-exec
                 ;; Avoid code execution.  See https://github.com/w0rp/ale/pull/1411
                 "--source" "'BEGIN{exit} END{exit 1}'"
                 "-f" "-"
                 "--lint"
                 null-device)
  :error-parser
  (flymake-rest-parse-rx
   ((error   bol (? "g") "awk: -:" line ": " (or "fatal" "error") ": " (message) eol)
    (warning bol (? "g") "awk: -:" line ": " "warning"            ": " (message) eol))))

(provide 'flymake-rest-awk-gawk)
