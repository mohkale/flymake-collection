;;; flymake-rest-pylint.el --- Pylint diagnostic function -*- lexical-binding: t -*-

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

;;;###autoload (autoload 'flymake-rest-pylint "flymake-rest-pylint")
(flymake-rest-define-enumerate flymake-rest-pylint
  "A Python syntax and style checker using Pylint.

This syntax checker requires Pylint 1.0 or newer.

See URL `https://www.pylint.org/'."
  :title "pylint"
  :pre-let ((python-exec (flymake-rest-executable-find "python3"))
            (pylint-exec (flymake-rest-executable-find "pylint"))
            (file-name (or (buffer-file-name flymake-rest-source)
                           "_")))
  :pre-check
  (progn
    (unless python-exec
      (error "Cannot find python executable"))
    (unless pylint-exec
      (error "Cannot find pylint executable")))
  :write-type 'pipe
  :command (list python-exec
                 "-m" "pylint"
                 "--reports=n"
                 "--output-format=json"
                 "--from-stdin"
                 file-name)
  :generator
  (car
   (flymake-rest-parse-json
    (buffer-substring-no-properties
     (point-min) (point-max))))
  :enumerate-parser
  (let-alist it
    (let ((loc (flymake-diag-region flymake-rest-source .line .column)))
      (list flymake-rest-source
            (car loc)
            (cdr loc)
            (pcase .type
              ;; See "pylint/utils.py"
              ((or "fatal" "error") :error)
              ((or "warning" "refactor" "convention") :warning)
              ((or "info" _) :note))
            (concat (propertize .message-id 'face 'flymake-rest-diag-id) " " .message)))))

(provide 'flymake-rest-pylint)

;;; flymake-rest-pylint.el ends here
