;;; flymake-collection-shellcheck.el --- Shellcheck diagnostic function -*- lexical-binding: t -*-

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

;; `flymake' syntax checker for POSIX shell (and bash) using shellcheck.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

(defcustom flymake-collection-shellcheck-follow-sources t
  "Whether to follow sources in `flymake-collection-shellcheck'."
  :type '(choice (const :tag "Follow source files" t)
                 (const :tag "Follow source files and lint them" lint)
                 (const :tag "Do not follow source files" nil))
  :group 'flymake-collection)

;;;###autoload (autoload 'flymake-collection-shellcheck "flymake-collection-shellcheck")
(flymake-collection-define-enumerate flymake-collection-shellcheck
  "A shell script syntax and style checker using Shellcheck.

See URL `https://github.com/koalaman/shellcheck/'."
  :title "shellcheck"
  :pre-let ((sh-exec (executable-find "shellcheck")))
  :pre-check (unless sh-exec
               (error "Cannot find shellcheck executable"))
  :write-type 'pipe
  :command `(,sh-exec
             "--format" "json"
             ,@(when-let ((sh (bound-and-true-p sh-shell)))
                 `("--shell" ,(symbol-name sh)))
             ,@(when flymake-collection-shellcheck-follow-sources
                 `("--external-sources"
                   ,@(when (eq flymake-collection-shellcheck-follow-sources 'lint)
                       '("--check-sourced"))))
             "-")
  :generator
  (car
   (flymake-collection-parse-json
    (buffer-substring-no-properties
     (point-min) (point-max))))
  :enumerate-parser
  (let-alist it
    (let ((loc (cons (car (flymake-diag-region flymake-collection-source .line .column))
                     (cdr (flymake-diag-region flymake-collection-source .endLine .endColumn)))))
      (list flymake-collection-source
            (car loc)
            (cdr loc)
            (pcase .level
              ("error" :error)
              ("warning" :warning)
              ((or "info" "style" _) :note))
            (concat (propertize (format "SC%s" .code) 'face 'flymake-collection-diag-id) " " .message)))))

(provide 'flymake-collection-shellcheck)

;;; flymake-collection-shellcheck.el ends here
