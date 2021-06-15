;;; flymake-rest-pycodestyle.el --- Pycodestyle diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest-define)

(eval-when-compile
  (require 'flymake-rest-rx))

;;;###autoload (autoload 'flymake-rest-pycodestyle "flymake-rest-pycodestyle")
(flymake-rest-define flymake-rest-pycodestyle
  :title "pycodestyle"
  :pre-let ((pycodestyle-exec (executable-find "pycodestyle")))
  :pre-check
  (unless pycodestyle-exec
    (error "Cannot find pycodestyle executable"))
  :write-type 'file
  :source-inplace t
  :command (list pycodestyle-exec fmqd-temp-file)
  :error-parser
  (flymake-rest-parse-rx
   ((error bol
           (file-name) ":" line ":" column ": " (id (or "E" "W") (one-or-more digit)) " " (message)
           eol))))

(provide 'flymake-rest-pycodestyle)
