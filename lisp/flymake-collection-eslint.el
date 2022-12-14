;;; flymake-collection-eslint.el --- ESLint diagnostic function -*- lexical-binding: t -*-

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

;; `flymake' syntax and style checker for Javascript using eslint.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

;;;###autoload (autoload 'flymake-collection-eslint "flymake-collection-eslint")
(flymake-collection-define-enumerate flymake-collection-eslint
  "A Javascript syntax and style checker using eslint.

See URL `https://eslint.org/'."
  :title "eslint"
  :pre-let ((eslint-exec (executable-find "eslint")))
  :pre-check (unless eslint-exec
               (error "Cannot find eslint executable"))
  :write-type 'pipe
  :command
  `(,eslint-exec
    "--format=json"
    "--stdin"
    ,@(when-let ((file (buffer-file-name flymake-collection-source)))
        (list "--stdin-filename" file)))
  :generator
  (alist-get
   'messages
   (caar
    (flymake-collection-parse-json
     (buffer-substring-no-properties
      (point-min) (point-max)))))
  :enumerate-parser
  (let-alist it
    (let* ((start-loc (flymake-diag-region flymake-collection-source .line .column))
           (loc (cons (car start-loc)
                      (cdr
                       (if (and .endLine .endColumn)
                           (flymake-diag-region flymake-collection-source
                                                .endLine (1- .endColumn))
                         start-loc)))))
      (list flymake-collection-source
            (car loc)
            (cdr loc)
            (pcase .severity
              (2 :error)
              (1 :warning)
              (_ :note))
            (concat "[" .ruleId "] " .message)))))

(provide 'flymake-collection-eslint)

;;; flymake-collection-eslint.el ends here
