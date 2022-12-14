;;; flymake-collection-less.el --- Less diagnostic function -*- lexical-binding: t -*-

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

;; `flymake' syntax checker for LESS using the less compiler.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

;;;###autoload (autoload 'flymake-collection-less "flymake-collection-less")
(flymake-collection-define-rx flymake-collection-less
  "A LESS syntax checker using lessc.

Requires lessc 1.4 or newer.

See URL `http://lesscss.org'."
  :title "lessc"
  :pre-let ((lessc-exec (executable-find "lessc")))
  :pre-check (unless lessc-exec
               (error "Cannot find lessc executable"))
  :write-type 'pipe
  :command (list lessc-exec  "--lint" "--no-color" "-")
  :regexps
  ((error bol (+ not-newline) ": " (message) " in - on line " line ", column " column ":" eol)))

(provide 'flymake-collection-less)


;;; flymake-collection-less.el ends here
