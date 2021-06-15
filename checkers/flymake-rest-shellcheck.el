;;; flymake-rest-shellcheck.el --- Shellcheck diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest-define)

(eval-when-compile
  (require 'flymake-rest-enumerate))

;;;###autoload (autoload 'flymake-rest-shellcheck "flymake-rest-shellcheck")
(flymake-rest-define flymake-rest-shellcheck
  "A shell script syntax and style checker using Shellcheck.

See URL `https://github.com/koalaman/shellcheck/'."
  :title "shellcheck"
  :pre-let ((sh-exec (executable-find "shellcheck")))
  :pre-check (unless sh-exec
               (error "Cannot find shellcheck executable"))
  :write-type 'pipe
  :command `(,sh-exec
             "--format" "json"
             ,@(when-let ((sh (bound-and-true-p sh-shell)))
                 `("--shell" ,(symbol-name sh)))
             "--external-sources"
             "-")
  :error-parser
  (flymake-rest-parse-enumerate
      (car
       (flymake-rest-parse-json
        (buffer-substring-no-properties
         (point-min) (point-max))))
    (let-alist it
      (let ((loc (cons (car (flymake-diag-region fmqd-source .line .column))
                       (cdr (flymake-diag-region fmqd-source .endLine .endColumn)))))
        (list fmqd-source
              (car loc)
              (cdr loc)
              (pcase .level
                ("error" :error)
                ("warning" :warning)
                ((or "info" "style" _) :note))
              (concat (propertize (format "SC%s" .code) 'face 'flymake-diag-id!) " " .message))))))

(provide 'flymake-rest-shellcheck)
