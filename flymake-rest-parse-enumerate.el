;;; flymakflymake-backend-parse-enumerate!to simplify checker creation -*- lexical-binding: t -*-

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
;; This file defines an error-parser for `flymake-rest-define' which can be used
;; to parse JSON output from the checker into flymake diagnostics. This works by
;; parsing the entire JSON input into a list of diagnostic related data, and then
;; iteratively parsing it into diagnostics.

;;; Code:

;;;###autoload
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

(provide 'flymake-rest-parse-enumerate)
