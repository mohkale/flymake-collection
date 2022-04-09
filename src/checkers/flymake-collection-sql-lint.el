;;; flymake-collection-sql-lint.el --- SQL diagnostic function -*- lexical-binding: t -*-

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

;; `flymake' syntax checker for sql using sql-lint.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

(defcustom flymake-collection-sql-lint-driver nil
  "The SQL driver to pass to sql-lint."
  :type '(choice
           (const :tag "Default" nil)
           (const :tag "MySQL" "mysql")
           (const :tag "PostgreSQL" "postgres"))
  :group 'flymake-collection)

;;;###autoload (autoload 'flymake-collection-sql-lint "flymake-collection-sql-lint")
(flymake-collection-define-rx flymake-collection-sql-lint
  "A SQL syntax checker using the sql-lint tool.

See URL `https://github.com/joereynolds/sql-lint'."
  :title "sql-lint"
  :pre-let ((lint-exec (executable-find "sql-lint")))
  :pre-check (unless lint-exec
               (error "Cannot find sql-lint executable"))
  :write-type 'pipe
  :command `(,lint-exec
             ,@(when flymake-collection-sql-lint-driver
                 `("--driver" ,flymake-collection-sql-lint-driver)))
  :regexps
  ((warning bol "stdin:" line " [sql-lint: " (id (one-or-more (any alnum "-"))) "] " (message) eol)))

(provide 'flymake-collection-sql-lint)

;;; flymake-collection-sql-lint.el ends here
