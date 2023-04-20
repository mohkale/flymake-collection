;;; flymake-collection-sqlint.el --- SQL diagnostic function -*- lexical-binding: t -*-

;; Copyright (c) 2021 Mohsin Kaleem

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; `flymake' syntax checker for sql using sqlint.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

;;;###autoload (autoload 'flymake-collection-sqlint "flymake-collection-sqlint")
(flymake-collection-define-rx flymake-collection-sqlint
  "A SQL syntax checker using the sqlint tool.

See URL `https://github.com/purcell/sqlint'."
  :title "sqlint"
  :pre-let ((lint-exec (executable-find "sqlint")))
  :pre-check (unless lint-exec
               (error "Cannot find sqlint executable"))
  :write-type 'pipe
  :command (list lint-exec)
  :regexps
  ((warning bol "stdin:" line ":" column ":WARNING "
     (message (one-or-more not-newline)
       (zero-or-more "\n"
         (one-or-more "  ")
         (one-or-more not-newline)))
     eol)
    (error bol "stdin:" line ":" column ":ERROR "
      (message (one-or-more not-newline)
        (zero-or-more "\n"
          (one-or-more "  ")
          (one-or-more not-newline)))
      eol)))

(provide 'flymake-collection-sqlint)

;;; flymake-collection-sqlint.el ends here

