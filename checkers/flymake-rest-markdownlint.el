;;; flymake-rest-markdownlint.el --- Markdownlint diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest-define)

(eval-when-compile
  (require 'flymake-rest-rx))

(defcustom flymake-rest-markdownlint-style nil
  "Path to the style config for markdownlint."
  :type 'string)

;;;###autoload (autoload 'flymake-rest-markdownlint "flymake-rest-markdownlint")
(flymake-rest-define flymake-rest-markdownlint
  "Markdown checker using mdl.

See URL `https://github.com/markdownlint/markdownlint'."
  :title "markdownlint"
  :pre-let ((mdl-exec (executable-find "mdl")))
  :pre-check (unless mdl-exec
               (error "Cannot find mdl executable"))
  :write-type 'pipe
  :command `(,mdl-exec
             ,@(and flymake-rest-markdownlint-style
                    `("--style" ,flymake-rest-markdownlint-style)))
  :error-parser
  (flymake-rest-parse-rx
   ((error bol "(stdin):" line ": " (id "MD" (+ digit)) " " (message) eol))))

(provide 'flymake-rest-markdownlint)
