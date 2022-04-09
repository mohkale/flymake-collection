;;; flymake-collection-rubocop.el --- Rubocop diagnostic function -*- lexical-binding: t -*-

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

;; `flymake' syntax checker for ruby using rubocop.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

(defcustom flymake-collection-rubocop-use-bundler t
  "When true use bundle exec for rubocop checks."
  :type 'boolean
  :group 'flymake-collection)

;;;###autoload (autoload 'flymake-collection-rubocop "flymake-collection-rubocop")
(flymake-collection-define-rx flymake-collection-rubocop
  "A Ruby syntax checker using rubocop.

See URL `https://github.com/rubocop/rubocop'."
  :title "rubocop"
  :pre-let ((rubocop-exec (executable-find "rubocop"))
            (file-name (or (buffer-file-name flymake-collection-source)
                           "-")))
  :pre-check (unless rubocop-exec
               (error "Cannot find rubocop executable"))
  :write-type 'pipe
  :command `(,@(or (and flymake-collection-rubocop-use-bundler
                        (locate-dominating-file (or (buffer-file-name flymake-collection-source)
                                                    (buffer-local-value 'default-directory
                                                                        flymake-collection-source)
                                                    default-directory)
                                                "Gemfile")
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
  :regexps
  ((error   bol (file-name) ":" line ":" column ": " (or "E" "F") ": " (? "[Correctable] ") (message) eol)
   (warning bol (file-name) ":" line ":" column ": " "C"          ": " (? "[Correctable] ") (message) eol)
   (note    bol (file-name) ":" line ":" column ": " "W"          ": " (? "[Correctable] ") (message) eol)))

(provide 'flymake-collection-rubocop)

;;; flymake-collection-rubocop.el ends here
