;;; flymake-rest-mypy.el --- MyPy diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest-define)

(eval-when-compile
  (require 'flymake-rest-parse-rx))

;;;###autoload (autoload 'flymake-rest-mypy "flymake-rest-mypy")
(flymake-rest-define flymake-rest-mypy
  "Mypy syntax and type checker.  Requires mypy>=0.580.

See URL `http://mypy-lang.org/'."
  :title "mypy"
  :pre-let ((mypy-exec (executable-find "mypy")))
  :pre-check (unless mypy-exec
               (error "Cannot find mypy executable"))
  :write-type 'file
  :source-inplace t
  :command (list mypy-exec
                 "--show-column-numbers"
                 "--no-error-summary"
                 "--no-color-output"
                 "--show-absolute-path"
                 "--show-error-codes"
                 fmqd-temp-file)
  :error-parser
  (flymake-rest-parse-rx
   ((error   bol (file-name) ":" line ":" column ": error: "   (message) eol)
    (warning bol (file-name) ":" line ":" column ": warning: " (message) eol)
    (note    bol (file-name) ":" line ":" column ": note: "    (message) eol))))

(provide 'flymake-rest-mypy)
