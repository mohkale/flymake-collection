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

(defun flymake-collection-vale-default-extension-function (buffer)
  "Default function for `flymake-collection-vale-extension-function'.
This function will return the actual extension of the file associated
with BUFFER.  If there is no extension, nil will be returned, causing
the omission of the \"--ext\" flag passed to vale."
  (let* ((file-name (buffer-file-name buffer))
         (extension (and file-name (file-name-extension file-name))))
    (when extension
      extension)))

(defcustom flymake-collection-vale-extension-function
  'flymake-collection-vale-default-extension-function
  "Function that returns the value of vale's \"--ext\" flag for the current file.
This function accepts one argument, a buffer, and returns the value of
the \"--ext\" flag (as a string), which is the extension of the file
associated with that buffer.  The associated extension determines which
checking rules vale uses according to the user's configuration(s).

If nil is returned, or the value of this option is nil, the \"--ext\"
flag is omitted.

The default function will return the actual extension of the file.  If
there is no extension, nil will be returned, omitting the \"--ext\"
flag.

Customizing this option can be useful if the user edits files without an
extension but would like them to be recognized as, say, org files."
  :type 'function
  :group 'flymake-collection)

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
             ,(when-let ((file-extension
                          (funcall flymake-collection-vale-extension-function flymake-collection-source)))
                (concat "--ext=." file-extension))
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
