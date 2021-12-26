;;; flymake-rest-clang.el --- Clang diagnostic function -*- lexical-binding: t -*-

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

;;; Code:

(require 'flymake)
(require 'flymake-rest)

(eval-when-compile
  (require 'flymake-rest-define))

(defvar flymake-rest-clang-args
  '("-pedantic" "-pedantic-errors")
  "Command line arguments always passed to `flymake-rest-clang'.")

(defvar flymake-rest-clang-include-path nil
  "Default include path for gcc in `flymake-rest-clang'.")

;;;###autoload (autoload 'flymake-rest-clang "flymake-rest-clang")
(flymake-rest-define-rx flymake-rest-clang
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
             "-iquote" ,(if-let ((file (buffer-file-name flymake-rest-source)))
                            (file-name-directory file)
                          (or (buffer-local-value 'default-directory
                                                  flymake-rest-source)
                              default-directory))
             ,@(cl-loop for it in flymake-rest-clang-include-path
                        collect (concat "-I" it))
             ,@flymake-rest-clang-args
             "-x" ,(pcase major-mode
                     ('c-mode "c")
                     ((or 'c++-mode _) "c++"))
             "-")
  :regexps
  ((error   bol "<stdin>:" line ":" column ": " (? "fatal ") "error" ": " (message) eol)
   (warning bol "<stdin>:" line ":" column ": " "warning"            ": " (message) eol)
   (note    bol "<stdin>:" line ":" column ": " "note"               ": " (message) eol)))

(provide 'flymake-rest-gcc)

;;; flymake-rest-clang.el ends here
