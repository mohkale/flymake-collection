;;; flymake-rest-hlint.el --- HLint diagnostic function -*- lexical-binding: t -*-

;;; Commentary:

;; `flymake' syntax checker for Haskell using hlint.

;;; Code:

(require 'flymake)
(require 'flymake-rest)

(eval-when-compile
  (require 'flymake-rest-define))

;;;###autoload (autoload 'flymake-rest-hlint "flymake-rest-hlint")
(flymake-rest-define-rx flymake-rest-hlint
  "A Haskell syntax and style checker using hlint.

See URL `https://github.com/ndmitchell/hlint'."
  :title "hlint"
  :pre-let ((hlint-exec (executable-find "hlint")))
  :pre-check (unless hlint-exec
               (error "Cannot find hlint executable"))
  :write-type 'file
  :command (list hlint-exec flymake-rest-temp-file)
  :regexps
  ((error   bol (file-name) ":" line ":" column (? "-" end-column) ": Error: "
     (message (one-or-more not-newline)
       (zero-or-more "\n"
        (one-or-more not-newline)))
     eol)
   (warning bol (file-name) ":" line ":" column (? "-" end-column) ": Warning: "
     (message (one-or-more not-newline)
       (zero-or-more "\n"
         (one-or-more not-newline)))
     eol)
   (note    bol (file-name) ":" line ":" column (? "-" end-column) ": Suggestion: "
     (message (one-or-more not-newline)
       (zero-or-more "\n"
         (one-or-more not-newline)))
     eol)))

(provide 'flymake-rest-hlint)

;;; flymake-rest-hlint.el ends here
