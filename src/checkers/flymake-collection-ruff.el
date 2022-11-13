;;; flymake-collection-ruff.el --- Ruff diagnostic function -*- lexical-binding: t -*-

;; Copyright (c) 2022 Fredrik Bergroth

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

;; `flymake' syntax checker for python using ruff.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

(defcustom flymake-collection-ruff-args nil
  "Command line arguments always passed to `flymake-collection-ruff'."
  :type '(repeat string)
  :group 'flymake-collection)

;;;###autoload (autoload 'flymake-collection-ruff "flymake-collection-ruff")
(flymake-collection-define-enumerate flymake-collection-ruff
  "A Python syntax and style checker using Ruff.

See URL `https://github.com/charliermarsh/ruff'."
  :title "ruff"
  :pre-let ((ruff-exec (executable-find "ruff")))
  :pre-check (unless ruff-exec
               (error "Cannot find ruff executable"))
  :write-type 'pipe
  :command `(,ruff-exec
             "--format" "json"
             ,@flymake-collection-ruff-args
             ,@(when-let ((file (buffer-file-name flymake-collection-source)))
                 (list "--stdin-filename" file))
             "-")

  :generator
  (car (flymake-collection-parse-json
   (buffer-substring-no-properties
    (point-min) (point-max))))
  :enumerate-parser
  (let-alist it
    (let ((loc (cons (car (flymake-diag-region
                           flymake-collection-source
                           .location.row .location.column))
                     (cdr (flymake-diag-region
                           flymake-collection-source
                           .end_location.row .end_location.column)))))
      (list flymake-collection-source
            (car loc)
            (cdr loc)
            :warning
            (concat (propertize .code 'face 'flymake-collection-diag-id) " " .message)))))

(provide 'flymake-collection-ruff)

;;; flymake-collection-ruff.el ends here
