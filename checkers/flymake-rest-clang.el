;;; flymake-rest-clang.el --- Clang diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest-define)

(eval-when-compile
  (require 'flymake-rest-rx))

(defvar flymake-rest-clang-args
  '("-pedantic" "-pedantic-errors")
  "Command line arguments always passed to `flymake-rest-clang'.")

(defvar flymake-rest-clang-include-path nil
  "Default include path for gcc in `flymake-rest-clang'.")

(flymake-rest-define flymake-clang
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
             "-iquote" ,(if-let ((file (buffer-file-name)))
                            (file-name-directory file)
                          default-directory)
             ,@(cl-loop for it in flymake-rest-clang-include-path
                        collect (concat "-I" it))
             ,@flymake-rest-clang-args
             "-x" ,(pcase major-mode
                     ('c-mode "c")
                     ((or 'c++-mode _) "c++"))
             "-")
  :error-parser
  (flymake-backend-parse-rx!
   ((error   bol "<stdin>:" line ":" column ": " (or "fatal" "error") ": " (message) eol)
    (warning bol "<stdin>:" line ":" column ": " "warning"            ": " (message) eol)
    (note    bol "<stdin>:" line ":" column ": " "note"               ": " (message) eol))))

(provide 'flymake-rest-gcc)
