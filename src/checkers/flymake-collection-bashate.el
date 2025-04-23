;;; flymake-collection-bashate.el --- Bashate diagnostic function -*- lexical-binding: t -*-

;; Copyright (c) 2025 Abdelhak Bougouffa

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

;; `flymake' backend for code style enforcement for Bash programs using "bashate"

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

;;; Custom variables

(defcustom flymake-collection-bashate-ignore-rules nil
  "A list of bashate rules to ignore."
  :type '(repeat string)
  :group 'flymake-collection)

(defcustom flymake-collection-bashate-max-line-length 'fill-column
  "The maximum line length in characters.
It can be an integer, nil or `fill-column'. When set to `fill-column',
we use the value of `fill-column'."
  :type '(choice (integer :tag "Maximum line length")
                 (const :tag "Use the value of `fill-column'" fill-column)
                 (const :tag "Unspecified" nil))
  :group 'flymake-collection)

(defcustom flymake-collection-bashate-executable "bashate"
  "The path or the command name of bashate."
  :type 'string
  :group 'flymake-collection)


;;;###autoload (autoload 'flymake-collection-bashate "flymake-collection-bashate")
(flymake-collection-define-rx flymake-collection-bashate
  "Bash code style linter tool using bashate."
  :title "bashate"
  :pre-let ((bashate-exec (executable-find flymake-collection-bashate-executable)))
  :pre-check (unless bashate-exec (error "Cannot find bashate executable"))
  :write-type 'file
  :command (append
            (list bashate-exec)
            (when flymake-collection-bashate-ignore-rules
              (list "--ignore" (mapconcat #'identity flymake-collection-bashate-ignore-rules ",")))
            (let ((len (pcase flymake-collection-bashate-max-line-length
                         ('fill-column fill-column)
                         ((pred numberp) flymake-collection-bashate-max-line-length))))
              (when len
                (list "--max-line-length" (number-to-string len))))
            (list flymake-collection-temp-file))
  :regexps
  ((error bol (* any) (file-name) ":" line ":" column ":" (+ space) "E040" (+ space) (message) eol)
   (warning bol (* any) (file-name) ":" line ":" column ":" (+ space) (group "E" (+ digit)) (+ space) (message) eol)))


(provide 'flymake-collection-bashate)
;;; flymake-collection-bashate.el ends here
