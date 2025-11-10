;;; flymake-collection-perlcritic.el --- perl diagnostic function using perlcritic -*- lexical-binding: t -*-

;; Copyright (c) 2025 Daniel Hennigar

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

;; `flymake' syntax checker for perl using Perl::Critic.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

(defcustom flymake-collection-perlcritic-args '()
    "Command line arguments will be passed to `flymake-collection-perlcritic.'.

See \"perlcritic --options\". Note that the \"--verbose\" flag is not
currently supported and will be stripped for correct output parsing."
    :type '(repeat :tag "Arguments" (string :tag "Argument"))
    :group 'flymake-collection)

(defcustom flymake-collection-perlcritic-severity 3
    "Set the severity level (or \"harshness\") of `perlcritic`.

Ranges from 1 (highest severity, most warnings)
to 5 (lowest severity, least warnings)."
    :type 'integer
    :group 'flymake-collection)

;;;###autoload (autoload 'flymake-collection-perlcritic "flymake-collection-perlcritic")
(flymake-collection-define-rx flymake-collection-perlcritic
  "This backend for `flymake' is based on Perl::Critic.

Provides additional warnings and is intended to accompany perl-mode's
default `perl-flymake', which uses the perl interpreter to catch syntax
errors. Requires Perl::Critic to be installed.
See URL `https://metacpan.org/pod/Perl::Critic'."
  :title "perlcritic"
  :pre-let ((perlcritic-exec (executable-find "perlcritic")))
  :pre-check (unless perlcritic-exec
               (user-error "Cannot find perlcritic executable"))
  :write-type 'pipe
  :command `(,perlcritic-exec
             "--severity" ,(number-to-string flymake-collection-perlcritic-severity)
             ;; enforce verbosity level 1 for correct regex matching
             "--verbose" "1"
             ,@(let ((args flymake-collection-perlcritic-args)
                     (pos (member "--verbose" flymake-collection-perlcritic-args)))
                   (if (and pos (string-match-p "^[0-9]+$" (cadr pos)))
                           (remove (cadr pos) (remove "--verbose" args))
                       (remove "--verbose" args)))
             "-")
  :regexps
  ;; only notes since perlcritic mostly provides stylistic suggestions
  ((note bol (file-name) ":" line ":" column ":" (message (+ nonl)) eol)))

(provide 'flymake-collection-perlcritic)

;;; flymake-collection-perlcritic.el ends here
