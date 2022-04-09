;;; flymake-collection-markdownlint.el --- Markdownlint diagnostic function -*- lexical-binding: t -*-

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

;; `flymake' syntax checker for markdown using markdownlint.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

(defcustom flymake-collection-markdownlint-style nil
  "Path to the style config for markdownlint."
  :type 'string
  :group 'flymake-collection)

;;;###autoload (autoload 'flymake-collection-markdownlint "flymake-collection-markdownlint")
(flymake-collection-define-rx flymake-collection-markdownlint
  "Markdown checker using mdl.

See URL `https://github.com/markdownlint/markdownlint'."
  :title "markdownlint"
  :pre-let ((mdl-exec (executable-find "mdl")))
  :pre-check (unless mdl-exec
               (error "Cannot find mdl executable"))
  :write-type 'pipe
  :command `(,mdl-exec
             ,@(and flymake-collection-markdownlint-style
                    `("--style" ,flymake-collection-markdownlint-style)))
  :regexps
  ((error bol "(stdin):" line ": " (id "MD" (+ digit)) " " (message) eol)))

(provide 'flymake-collection-markdownlint)

;;; flymake-collection-markdownlint.el ends here
