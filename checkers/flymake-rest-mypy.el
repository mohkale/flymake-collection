;;; flymake-rest-mypy.el --- MyPy diagnostic function -*- lexical-binding: t -*-

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
  (require 'flymake-rest-define)
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

;;; flymake-rest-mypy.el ends here
