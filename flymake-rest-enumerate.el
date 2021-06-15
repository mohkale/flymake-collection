;;; flymakflymake-backend-parse-enumerate!to simplify checker creation -*- lexical-binding: t -*-

;;; Commentary:
;; TODO
;; TODO: license

;;; Code:

(defmacro flymake-rest-parse-enumerate (gen &rest body)
  "Error parser for `flymake-backend-define' which parses all of
the diagnostics at once using GEN and then preparing them one-at-a-time
with BODY.

The value of the current entry from GEN in BODY will be set to the variable
`it'. BODY should evaluate to a form that can be passed to
`flymake-make-diagnostic'."
  (declare (indent 1))
  (let ((context-var (intern "fmqd-context")))
    `(progn
       (unless (alist-get 'enumerated ,context-var)
         (push (cons 'entries ,gen) ,context-var)
         (push '(enumerated t) ,context-var))
       (let (it res)
         ;; While we haven't found a new diagnostic to return, BUT there're
         ;; still diagnostics that can be found in the parsed checker output.
         (while (and (not res)
                     (setq it (pop (alist-get 'entries ,context-var))))
           (setq res ,@body))
         res))))

(provide 'flymake-rest-enumerate)
