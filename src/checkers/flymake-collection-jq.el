;;; flymake-collection-jq.el --- jq diagnostic function -*- lexical-binding: t -*-

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

;; `flymake' syntax checker for JSON using jq.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

;;;###autoload (autoload 'flymake-collection-jq "flymake-collection-jq")
(flymake-collection-define-rx flymake-collection-jq
  "JSON checker using the jq tool.

This checker accepts multiple consecutive JSON values in a
single input, which is useful for jsonlines data.

See URL `https://stedolan.github.io/jq/'."
  :title "jq"
  :pre-let ((jq-exec (executable-find "jq")))
  :pre-check (unless jq-exec
               (error "Cannot find jq executable"))
  :write-type 'pipe
  :command (list jq-exec "." "-" null-device)
  :regexps
  ((error bol "parse error: " (message) " at line " line ", column " column eol)))

(provide 'flymake-collection-jq)

;;; flymake-collection-jq.el ends here
