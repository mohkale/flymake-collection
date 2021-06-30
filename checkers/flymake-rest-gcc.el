;;; flymake-rest-gcc.el --- GCC diagnostic function -*- lexical-binding: t -*-

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
(require 'flymake-rest-define)

(eval-when-compile
  (require 'flymake-rest-parse-rx))

(defcustom flymake-rest-gcc-args
  '("-pedantic" "-pedantic-errors")
  "Command line arguments always passed to `flymake-rest-gcc'."
  :type 'list
  :group 'flymake-rest)

(defcustom flymake-rest-gcc-include-path nil
  "Default include path for gcc in `flymake-rest-gcc'."
  :type 'list
  :group 'flymake-rest)

;;;###autoload (autoload 'flymake-rest-gcc "flymake-rest-gcc")
(flymake-rest-define flymake-rest-gcc
  "A C/C++ syntax checker using GCC.

Requires GCC 4.4 or newer.  See URL `https://gcc.gnu.org/'."
  :title "gcc"
  :pre-let ((gcc-exec (executable-find "gcc")))
  :pre-check (unless gcc-exec
               (error "Cannot find gcc executable"))
  :write-type 'pipe
  :command `(,gcc-exec
             "-fshow-column"
             "-iquote" ,(if-let ((file (buffer-file-name)))
                            (file-name-directory file)
                          default-directory)
             ,@(cl-loop for it in flymake-rest-gcc-include-path
                        collect (concat "-I" it))
             ,@flymake-rest-gcc-args
             "-x" ,(pcase major-mode
                     ('c-mode "c")
                     ((or 'c++-mode _) "c++"))
             ;; GCC performs full checking only when actually compiling, so
             ;; `-fsyntax-only' is not enough. Just let it generate assembly
             ;; code.
             "-S" "-o" ,null-device
             "-")
  :error-parser
  (flymake-rest-parse-rx
   ((error   bol "<stdin>:" line ":" column ": " (or "fatal" "error") ": " (message) eol)
    (warning bol "<stdin>:" line ":" column ": " "warning"            ": " (message) eol)
    (note    bol "<stdin>:" line ":" column ": " "note"               ": " (message) eol))))

(provide 'flymake-rest-gcc)

;;; flymake-rest-gcc.el ends here
