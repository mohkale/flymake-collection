;;; flymake-rest-html-tidy.el --- Tidy-HTML5 diagnostic function -*- lexical-binding: t -*-

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

;;; Code:

(require 'flymake)
(require 'flymake-rest)

(eval-when-compile
  (require 'flymake-rest-define))

;;;###autoload (autoload 'flymake-rest-html-tidy "flymake-rest-html-tidy")
(flymake-rest-define-rx flymake-rest-html-tidy
  "A HTML syntax and style checker using Tidy.

See URL `https://github.com/htacg/tidy-html5'."
  :title "tidy"
  :pre-let ((tidy-exec (flymake-rest-executable-find "tidy")))
  :pre-check (unless tidy-exec
               (error "Cannot find tidy executable"))
  :write-type 'pipe
  :command `(,tidy-exec "-lang" "en" "-e" "-q")
  :regexps
  ((error   bol "line " line " column " column " - Error: "   (message) eol)
   (warning bol "line " line " column " column " - Warning: " (message) eol)))

(provide 'flymake-rest-html-tidy)

;;; flymake-rest-html-tidy.el ends here
