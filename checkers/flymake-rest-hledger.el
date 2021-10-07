;;; flymake-rest-hledger.el --- hledger diagnostic function -*- lexical-binding: t -*-

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
  (require 'flymake-rest-define)
  (require 'flymake-rest-parse-rx))

(defcustom flymake-rest-hledger-strict nil
  "Whether to enable strict hledger checks.

See URL https://hledger.org/hledger.html#strict-mode"
  :group 'flymake-rest
  :group 'hledger-mode
  :type 'boolean)

(defcustom flymake-rest-hledger-checks nil
  "List of additional checks to run.

Checks include: accounts, commodities, ordereddates, payees and
uniqueleafnames. More information at URL
https://hledger.org/hledger.html#check."
  :group 'flymake-rest
  :group 'hledger-mode
  :type '(repeat string))

;;;###autoload (autoload 'flymake-rest-hledger "flymake-rest-hledger")
(flymake-rest-define flymake-rest-hledger
  "A checker for hledger journals, showing unmatched balances and failed checks."
  ;; :title "hledger"
  :pre-let ((hledger-exec (or (and (bound-and-true-p ledger-binary-path)
                                   (string-suffix-p "hledger" ledger-binary-path)
                                   (file-executable-p ledger-binary-path)
                                   ledger-binary-path)
                              (executable-find "hledger"))))
  :pre-check (unless hledger-exec
               (error "Cannot find hledger executable"))
  :write-type 'file
  :source-inplace t
  :command `(,hledger-exec
             "-f" ,flymake-rest-temp-file
             "--auto" "check"
             ,@(when flymake-rest-hledger-strict
                 (list "--strict"))
             ,@flymake-rest-hledger-checks)
  :error-parser
  (flymake-rest-parse-rx
   ;; Taken verbatim from [[https://github.com/DamienCassou/flycheck-hledger/blob/master/flycheck-hledger.el][flycheck-hledger]], this is way too involved to vet fully.
   ((error
     (or
      ;; Used for an unbalanced transaction:
      (and line-start "hledger: \"" (file-name) "\" (lines " line "-" end-line ")\n"
           (message (zero-or-more line-start (zero-or-more not-newline) "\n")) "\n")
      ;; Used for invalid balance assertion:
      (and line-start "hledger: balance assertion: \"" (file-name) "\" (line " line ", column " column ")\n"
           "transaction:\n"
           (message (zero-or-more line-start (zero-or-more not-newline) "\n")) "\n")
      ;; Used for invalid regular expression:
      (and line-start "hledger: " (message "this regular expression" (zero-or-more not-newline)) "\n")
      ;; Used for an undeclared payee:
      (and line-start "Error: " (message) "\n"
           "at: \"" (file-name) "\" (lines " line "-" end-line ")\n")
      ;; Used for unordered dates:
      (and line-start "Error: " (message) "\n"
           "at \"" (file-name) "\" (lines " line "-" end-line "):\n")
      ;; Used for duplicate leaf names:
      (and line-start "Error: " (message) "\n")
      ;; Used for an undeclared account:
      (and line-start "hledger: " (message) "\n"
           "in transaction at: \"" (file-name) "\" (lines " line "-" end-line ")\n")
      ;; Used for parse errors and invalid dates:
      (and line-start "hledger: " (file-name) ":" line ":" column ":\n"
           (message (zero-or-more line-start (zero-or-more not-newline) "\n")) "\n"))))))

(provide 'flymake-rest-hledger)

;;; flymake-rest-hledger.el ends here
