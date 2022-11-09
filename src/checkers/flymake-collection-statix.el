;;; flymake-collection-statix.el --- Statix diagnostic function -*- lexical-binding: t -*-

;; Copyright (c) 2022 Valeriy Litkovskyy

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

;; This file provides a flymake backend that lets the user analyze nix code
;; using statix.

;;;; Tips

;; + You can customize `flymake-collection-statix-args' to your liking.

;;;; Credits

;; This diagnostic would not have been possible without the statix[1] program.

;;  [1] https://github.com/nerdypepper/statix

;;; Code:

;;;; Requirements

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

;;;; Customization

(defcustom flymake-collection-statix-args nil
  "Additional statix check arguments."
  :type '(repeat :tag "Arguments" (string :tag "Argument"))
  :group 'flymake-collection)

;;;; Functions

;;;;; Public

;;;###autoload (autoload 'flymake-collection-statix "flymake-collection-statix")
(flymake-collection-define-enumerate flymake-collection-statix
  "A Nix syntax and style checker using statix.

See URL `https://github.com/nerdypepper/statix'."
  :title "statix"
  :pre-let ((statix-exec (executable-find "statix")))
  :pre-check (unless statix-exec
               (error "Cannot find statix executable"))
  :command `(,statix-exec "check" "--format" "json" "--stdin"
                          ,@flymake-collection-statix-args)
  :generator
  (let ((data (list (cons 'buf flymake-collection-source))))
    (mapcan
     (lambda (report)
       (let-alist report
         (let ((data (cons (cons 'severity .severity) data)))
           (mapcar (apply-partially #'append data) .diagnostics))))
     (cdr (assq 'report (car (flymake-collection-parse-json
                              (buffer-substring-no-properties
                               (point-min) (point-max))))))))
  :enumerate-parser
  (let-alist it
    (list .buf
          (car (flymake-diag-region .buf .at.from.line .at.from.column))
          (car (flymake-diag-region .buf .at.to.line .at.to.column))
          (flymake-collection-statix--make-type it)
          .message)))

;;;;; Private

(defun flymake-collection-statix--make-type (it)
  "Make diagnostic type from IT.
See `flymake-collection-define-enumerate' for IT.  IT must
contain severity."
  (let-alist it
    (pcase-exhaustive .severity
      ("Warn" :warning)
      ("Error" :error)
      ("Hint" :note))))

(provide 'flymake-collection-statix)

;;; flymake-collection-statix.el ends here
