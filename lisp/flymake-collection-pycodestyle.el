;;; flymake-collection-pycodestyle.el --- Pycodestyle diagnostic function -*- lexical-binding: t -*-

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

;; `flymake' syntax checker for python using pycodestyle.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

;;;###autoload (autoload 'flymake-collection-pycodestyle "flymake-collection-pycodestyle")
(flymake-collection-define-rx flymake-collection-pycodestyle
  "Python style guide checker.

See URL `https://github.com/PyCQA/pycodestyle'."
  :title "pycodestyle"
  :pre-let ((pycodestyle-exec (executable-find "pycodestyle")))
  :pre-check
  (unless pycodestyle-exec
    (error "Cannot find pycodestyle executable"))
  :write-type 'file
  :source-inplace t
  :command (list pycodestyle-exec flymake-collection-temp-file)
  :regexps
  ((error bol (file-name) ":" line ":" column ": " (id (or "E" "W") (one-or-more digit)) " " (message) eol)))

(provide 'flymake-collection-pycodestyle)

;;; flymake-collection-pycodestyle.el ends here
