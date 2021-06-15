;;; flymake-rest-pylint.el --- Pylint diagnostic function -*- lexical-binding: t -*-

;;; Code:

(require 'flymake)
(require 'flymake-rest)

(eval-when-compile
  (require 'flymake-rest-define)
  (require 'flymake-rest-parse-enumerate))

;;;###autoload (autoload 'flymake-rest-pylint "flymake-rest-pylint")
(flymake-rest-define flymake-rest-pylint
  :title "pylint"
  :pre-let ((python-exec (executable-find "python3"))
            (pylint-exec (executable-find "pylint")))
  :pre-check
  (progn
    (unless python-exec
      (error "Cannot find python executable"))
    (unless pylint-exec
      (error "Cannot find pylint executable")))
  :write-type 'file
  :source-inplace t
  :command (list python-exec
                 "-m" "pylint"
                 "--reports=n"
                 "--output-format=json"
                 fmqd-temp-file)
  :error-parser
  (flymake-rest-parse-enumerate
      (car
       (flymake-rest-parse-json
        (buffer-substring-no-properties
         (point-min) (point-max))))
    (let-alist it
      (let ((loc (flymake-diag-region fmqd-source .line .column)))
        (list fmqd-source
              (car loc)
              (cdr loc)
              (pcase .type
                ;; See "pylint/utils.py"
                ((or "fatal" "error") :error)
                ((or "warning" "refactor" "convention") :warning)
                ((or "info" _) :note))
              (concat (propertize .message-id 'face 'flymake-rest-diag-id) " " .message))))))

(provide 'flymake-rest-pylint)
