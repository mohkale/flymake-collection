;;; flymake-collection-clang.el --- Clang diagnostic function -*- lexical-binding: t -*-

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

;; `flymake' syntax checker for c using Clang.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

(defvar flymake-collection-clang-args
  '("-pedantic" "-pedantic-errors")
  "Command line arguments always passed to `flymake-collection-clang'.")

(defvar flymake-collection-clang-include-path nil
  "Default include path for clang in `flymake-collection-clang'.")

;;;###autoload (autoload 'flymake-collection-clang "flymake-collection-clang")
(flymake-collection-define-rx flymake-collection-clang
  "A C/C++ syntax checker using Clang.

See URL `http://clang.llvm.org/'."
  :title "clang"
  :pre-let ((clang-exec (executable-find "clang")))
  :pre-check (unless clang-exec
               (error "Cannot find clang executable"))
  :write-type 'pipe
  :command `(,clang-exec
             "-fsyntax-only"
             "-fno-color-diagnostics"                                 ; Do not include color codes in output
             "-fno-caret-diagnostics"                                 ; Do not visually indicate the source
             "-fno-diagnostics-show-option"                           ; Do not show the corresponding
             "-iquote" ,(if-let ((file (buffer-file-name flymake-collection-source)))
                            (file-name-directory file)
                          (or (buffer-local-value 'default-directory
                                                  flymake-collection-source)
                              default-directory))
             ,@(cl-loop for it in flymake-collection-clang-include-path
                        collect (concat "-I" it))
             ,@flymake-collection-clang-args
             "-x" ,(pcase major-mode
                     ('c-mode "c")
                     ((or 'c++-mode _) "c++"))
             "-")
  :regexps
  ((error   bol "<stdin>:" line ":" column ": " (? "fatal ") "error" ": " (message) eol)
   (warning bol "<stdin>:" line ":" column ": " "warning"            ": " (message) eol)
   (note    bol "<stdin>:" line ":" column ": " "note"               ": " (message) eol)))

(provide 'flymake-collection-clang)

;;; flymake-collection-clang.el ends here
