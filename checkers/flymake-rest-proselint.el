;;; flymake-rest-proselint.el --- Proselint diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest)

(eval-when-compile
  (require 'flymake-rest-define)
  (require 'flymake-rest-parse-enumerate))

;;;###autoload (autoload 'flymake-rest-proselint "flymake-rest-proselint")
(flymake-rest-define flymake-rest-proselint
  "Flymake checker using Proselint.

See URL `http://proselint.com/'."
  :title "proselint"
  :pre-let ((proselint-exec (executable-find "proselint")))
  :pre-check (unless proselint-exec
               (error "Cannot find proselint executable"))
  :write-type 'pipe
  :command `(,proselint-exec "--json" "-")
  :error-parser
  (flymake-rest-parse-enumerate
      (alist-get 'errors
       (alist-get 'data
        (car
         (flymake-rest-parse-json
          (buffer-substring-no-properties
           (point-min) (point-max))))))
    (let-alist it
      (let ((loc (cons (car (flymake-diag-region fmqd-source .line .column))
                       (cdr (flymake-diag-region fmqd-source .endLine .endColumn)))))
        (list fmqd-source
              .start
              .end
              (pcase .severity
                ("suggestion" :note)
                ("warning" :warning)
                ((or "error" _) :error))
              (concat (propertize .check 'face 'flymake-diag-id!) " " .message))))))

(provide 'flymake-rest-proselint)
