;;; flymake-rest-rubocop.el --- Rubocop diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest)

(eval-when-compile
  (require 'flymake-rest-define)
  (require 'flymake-rest-parse-rx))

(defcustom flymake-rest-rubocop-use-bundler t
  :type 'boolean)

;;;###autoload (autoload 'flymake-rest-rubocop "flymake-rest-rubocop")
(flymake-rest-define flymake-rest-rubocop
  "A Ruby syntax checker using rubocop.

See URL `https://github.com/rubocop/rubocop'."
  :title "rubocop"
  :pre-let ((rubocop-exec (executable-find "rubocop"))
            (file-name (or (buffer-file-name)
                           "-")))
  :pre-check (unless rubocop-exec
               (error "Cannot find rubocop executable"))
  :write-type 'pipe
  :command `(,@(or (and flymake-rest-rubocop-use-bundler
                        (locate-dominating-file (buffer-file-name) "Gemfile")
                        (if-let ((bundler-exec (executable-find "bundler")))
                            (list bundler-exec "exec" "rubocop")
                          (flymake-log :warning "In bundler controlled project but bundler not installed")))
                   (list rubocop-exec))
             "--display-cop-names"
             "--force-exclusion"
             "--format" "emacs"
             ;; Explicitly disable caching to prevent Rubocop 0.35.1 and earlier
             ;; from caching standard input.  Later versions of Rubocop
             ;; automatically disable caching with --stdin, see
             ;; https://github.com/flycheck/flycheck/issues/844 and
             ;; https://github.com/bbatsov/rubocop/issues/2576
             "--cache" "false"
             ;; Rubocop takes the original file name as argument when reading
             ;; from standard input
             "--stdin" ,file-name)
  :error-parser
  (flymake-rest-parse-rx
   ((error   bol (file-name) ":" line ":" column ": " (or "E" "F") ": " (? "[Correctable] ") (message) eol)
    (warning bol (file-name) ":" line ":" column ": " "C"          ": " (? "[Correctable] ") (message) eol)
    (note    bol (file-name) ":" line ":" column ": " "W"          ": " (? "[Correctable] ") (message) eol))))

(provide 'flymake-rest-rubocop)
