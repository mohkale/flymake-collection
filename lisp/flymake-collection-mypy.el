;;; flymake-collection-mypy.el --- MyPy diagnostic function -*- lexical-binding: t -*-

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

;; `flymake' syntax checker for python using mypy.

;;; Code:

(require 'project)
(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

(defcustom flymake-collection-mypy-args
  '("--show-error-codes")
  "Command line arguments always passed to `flymake-collection-mypy'."
  :type '(repeat (string :tag "Arg"))
  :group 'flymake-collection)

(defcustom flymake-collection-mypy-project-root
  '(("mypy.ini" "project.toml" "setup.cfg") project-root default-directory)
  "Method to set project root."
  :type '(repeat :tag "Run mypy from the first choice that succeeds"
                 (choice (const :tag "The buffer default-directory" default-directory)
                         (const :tag "The current project root" project-root)
                         (directory :tag "A specific directory")
                         (repeat :tag "The first ancestor directory containing"
                                 :value ("mypy.ini" "pyproject.toml" "setup.cfg")
                                 (string :tag "File name"))))
  :group 'flymake-collection
  :safe 'listp)

(defun flymake-collection-mypy--locate-dominating-files (buffer files)
  "Find ancestor directory of `BUFFER' containing any of `FILES'."
  (let* ((start (if-let ((file (buffer-file-name buffer)))
                    (file-name-directory file)
                  (buffer-local-value 'default-directory buffer)))
         (regex (mapconcat 'regexp-quote files "\\|"))
         (root (locate-dominating-file
                start (lambda (dir) (directory-files dir nil regex t)))))
    root))

(defun flymake-collection-mypy--default-directory (buffer)
  "Find a directory from which to run mypy to check `BUFFER'.
Try each method specified in `flymake-collection-mypy-project-root' in
order and returns the first non-nil result."
  (cl-dolist (spec flymake-collection-mypy-project-root)
    (when-let
        ((res (cond
               ((eq spec 'default-directory)
                (buffer-local-value 'default-directory buffer))
               ((eq spec 'project-root)
                (when-let ((proj (project-current)))
                  (project-root proj)))
               ((listp spec)
                (flymake-collection-mypy--locate-dominating-files
                 buffer spec))
               ((stringp spec) spec))))
      (cl-return res))))


;;;###autoload (autoload 'flymake-collection-mypy "flymake-collection-mypy")
(flymake-collection-define-rx flymake-collection-mypy
  "Mypy syntax and type checker.  Requires mypy>=0.580.

See URL `http://mypy-lang.org/'."
  :title "mypy"
  :pre-let ((mypy-exec (executable-find "mypy"))
            (default-directory (flymake-collection-mypy--default-directory
                                flymake-collection-source)))
  :pre-check (progn
               (flymake-log :debug "Working dir is %s" default-directory)
               (unless mypy-exec
                 (error "Cannot find mypy executable"))
               (unless default-directory
                 (error "Default dir is nil: check `flymake-collection-mypy-project-root'")))
  :write-type 'file
  :source-inplace t
  :command `(,mypy-exec
             "--show-column-numbers"
             "--show-absolute-path"
             "--no-error-summary"
             "--no-color-output"
             ,@flymake-collection-mypy-args
             ,@(if-let ((source-file (buffer-file-name
                                      flymake-collection-source))
                        ((file-exists-p source-file)))
                   (list
                    "--shadow-file" source-file flymake-collection-temp-file
                    source-file)
                 (list flymake-collection-temp-file)))
  ;; Temporary file cannot start with a dot for mypy, see
  ;; https://github.com/mohkale/flymake-collection/pull/9
  :temp-file-prefix "flymake_mypy_"
  :regexps
  ((error   bol (file-name) ":" line ":" column ": error: "
            (message) (opt "  [" (id (* graph)) "]") eol)
   (warning bol (file-name) ":" line ":" column ": warning: "
            (message) (opt "  [" (id (* graph)) "]") eol)
   (note    bol (file-name) ":" line ":" column ": note: "
            (message) (opt "  [" (id (* graph)) "]") eol)))

(provide 'flymake-collection-mypy)

;;; flymake-collection-mypy.el ends here
