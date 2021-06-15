;;; flymake-rest-gcc.el --- GCC diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest-define)

(eval-when-compile
  (require 'flymake-rest-rx))

(defcustom flymake-rest-gcc-args
  '("-pedantic" "-pedantic-errors")
  "Command line arguments always passed to `flymake-rest-gcc'.")

(defcustom flymake-rest-gcc-include-path nil
  "Default include path for gcc in `flymake-rest-gcc'.")

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
