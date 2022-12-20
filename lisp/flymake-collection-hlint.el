;;; flymake-collection-hlint.el --- HLint diagnostic function -*- lexical-binding: t -*-

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

;; `flymake' syntax checker for Haskell using hlint.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

;;;###autoload (autoload 'flymake-collection-hlint "flymake-collection-hlint")
(flymake-collection-define-rx flymake-collection-hlint
  "A Haskell syntax and style checker using hlint.

See URL `https://github.com/ndmitchell/hlint'."
  :pre-let ((hlint-exec (executable-find "hlint")))
  :pre-check (unless hlint-exec
               (error "Cannot find hlint executable"))
  :write-type 'pipe
  :command (list hlint-exec "-" "--no-exit-code")
  :regexps
  ((error bol (file-name) ":" line ":" column (? "-" end-column) ": Error: "
     (message (one-or-more not-newline)
       (zero-or-more "\n"
        (one-or-more not-newline)))
     eol)
   (warning bol (file-name) ":" line ":" column (? "-" end-column) ": Warning: "
     (message (one-or-more not-newline)
       (zero-or-more "\n"
         (one-or-more not-newline)))
     eol)
   (note bol (file-name) ":" line ":" column (? "-" end-column) ": Suggestion: "
     (message (one-or-more not-newline)
       (zero-or-more "\n"
         (one-or-more not-newline)))
     eol)))

(provide 'flymake-collection-hlint)

;;; flymake-collection-hlint.el ends here
