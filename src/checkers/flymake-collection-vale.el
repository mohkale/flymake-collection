;;; flymake-collection-vale.el --- vale diagnostic function -*- lexical-binding: t -*-

;; Copyright (c) 2024 Mohsin Kaleem

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

;; `flymake' syntax checker for prose using vale.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

;;;###autoload (autoload 'flymake-collection-vale "flymake-collection-vale")
(flymake-collection-define-enumerate flymake-collection-vale
  "A prose syntax and style checker using vale.

See https://vale.sh/."
  :title "vale"
  :pre-let ((vale-exec (executable-find "vale")))
  :pre-check (unless vale-exec
               (error "Cannot find vale executable"))
  :write-type 'pipe
  :command `(,vale-exec
             ,@(let* ((file-name (buffer-file-name flymake-collection-source))
                      (extension (and file-name (file-name-extension file-name))))
                 (when extension
                   (list (concat "--ext=." extension))))
             "--output=JSON")
  :generator
  (cdaar
   (flymake-collection-parse-json
    (buffer-substring-no-properties
     (point-min) (point-max))))
  :enumerate-parser
  (let-alist it
    `(,flymake-collection-source
      ,@(with-current-buffer flymake-collection-source
          (save-excursion
            (goto-char (point-min))
            (unless (and (eq .Line 1)
                         (not (bolp)))
              (forward-line (1- .Line)))
            (list (+ (point) (1- (car .Span)))
                  (+ (point) (cadr .Span)))))
      ,(pcase .Severity
         ("suggestion" :note)
         ("warning" :warning)
         ((or "error" _) :error))
      ,(concat (propertize (concat "[" .Check "]") 'face 'flymake-collection-diag-id) " "
               .Message))))

(provide 'flymake-collection-vale)
;;; flymake-collection-vale.el ends here
