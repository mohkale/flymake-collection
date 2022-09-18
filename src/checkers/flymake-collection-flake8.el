;;; flymake-collection-flake8.el --- Flake8 diagnostic function -*- lexical-binding: t -*-

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

;; `flymake' syntax checker for python using flake8.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

(defcustom flymake-collection-flake8-args nil
  "Command line arguments always passed to `flymake-collection-flake8'."
  :type '(repeat string)
  :group 'flymake-collection)

;;;###autoload (autoload 'flymake-collection-flake8 "flymake-collection-flake8")
(flymake-collection-define-rx flymake-collection-flake8
  "A Python syntax and style checker using Flake8.

This syntax checker requires Flake8 3.0 or newer.
See URL `https://flake8.readthedocs.io/'."
  :title "flake8"
  :pre-let ((flake8-exec (executable-find "flake8")))
  :pre-check (unless flake8-exec
               (error "Cannot find flake8 executable"))
  :write-type 'pipe
  :command `(,flake8-exec
             ,@flymake-collection-flake8-args
             ,@(when-let ((file (buffer-file-name flymake-collection-source)))
                 (list "--stdin-display-name" file))
             "-")
  :regexps
  ((warning bol (file-name) ":" line ":" column ": " (id (one-or-more alnum)) " " (message) eol)))

(provide 'flymake-collection-flake8)

;;; flymake-collection-flake8.el ends here
