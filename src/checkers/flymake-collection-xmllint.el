;;; flymake-collection-xmllint.el --- XML diagnostic function -*- lexical-binding: t; -*-

;; Copyright (C) 2021  mohsin kaleem

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

;; `flymake' syntax checker for XML using xmllint.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

;;;###autoload (autoload 'flymake-collection-xmllint "flymake-collection-xmllint")
(flymake-collection-define-rx flymake-collection-xmllint
  "A XML syntax checker and validator using the xmllint utility.

The xmllint is part of libxml2, see URL `http://www.xmlsoft.org/'."
  :title "xmllint"
  :pre-let ((xmllint-exec (executable-find "xmllint")))
  :pre-check
  (unless xmllint-exec
    (error "Cannot find xmllint executable"))
  :write-type 'pipe
  :command `(,xmllint-exec "--noout" "-")
  :regexps
  ((error bol "-:" line ": " (message) eol)))

(provide 'flymake-collection-xmllint)

;;; flymake-collection-xmllint.el ends here
