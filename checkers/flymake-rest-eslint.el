;;; flymake-rest-eslint.el --- ESLint diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest-define)

(eval-when-compile
  (require 'flymake-rest-enumerate))

;;;###autoload (autoload 'flymake-rest-eslint "flymake-rest-eslint")
(flymake-rest-define flymake-rest-eslint
  "A Javascript syntax and style checker using eslint.

See URL `https://eslint.org/'."
  :title "eslint"
  :pre-let ((eslint-exec (executable-find "eslint")))
  :pre-check (unless eslint-exec
               (error "Cannot find eslint executable"))
  :write-type 'pipe
  :command
  `(,eslint-exec
    "--format=json"
    "--stdin"
    ,@(when-let ((file (buffer-file-name)))
        (list "--stdin-filename" file)))
  :error-parser
  (flymake-rest-parse-enumerate
      (alist-get
       'messages
       (caar
        (flymake-rest-parse-json
         (buffer-substring-no-properties
          (point-min) (point-max)))))
    (let-alist it
      (let ((loc (cons (car (flymake-diag-region fmqd-source .line .column))
                       (cdr (flymake-diag-region fmqd-source .endLine .endColumn)))))
        (list fmqd-source
              (car loc)
              (cdr loc)
              (pcase .severity
                (2 :error)
                (1 :warning)
                (_ :note))
              (concat "[" .ruleId "] " .message))))))

(provide 'flymake-rest-eslint)
