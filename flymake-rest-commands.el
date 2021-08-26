;;; flymake-rest-commands.el --- Helpful commands for working with flymake-rest -*- lexical-binding: t -*-

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
(require 'flymake-rest-hook)

(defun flymake-rest-change-checker--cands (all-modes)
  (cl-remove-duplicates
   (cl-loop for (mode . checkers) in
            (if all-modes
                flymake-rest-config
              (list (assoc major-mode flymake-rest-config)))
            append
            (cl-loop for it in checkers
                     with checker = nil
                     do (setq checker (if (symbolp it)
                                          it
                                        (car it)))
                     with exists = nil
                     do (setq exists (or (member checker flymake-diagnostic-functions)
                                         (when-let ((state (gethash checker flymake--backend-state)))
                                           (not (flymake--backend-state-disabled state)))))
                     when checker
                     collect (list (symbol-name checker)
                                   mode checker exists)))
   :test (lambda (a b) (string-equal (car a) (car b)))))

(defun flymake-rest-change-checker--read-checkers (&optional all-modes)
  (let* ((cands (flymake-rest-change-checker--cands all-modes))
         (group-function (lambda (cand transform)
                           (if transform
                               cand
                             (symbol-name (cadr (assoc cand cands))))))
         (affix-function (lambda (cands-keys)
                           (cl-loop
                            for cand in cands-keys collect
                            (list cand (if (cadddr (assoc cand cands)) "-" "+") nil)))))
    (unless cands
      (user-error "No diagnostic functions configured for the current buffer"))
    (mapcar
     (lambda (it)
       (assoc it cands))
     (completing-read-multiple
      "Diagnostic function: "
      (lambda (str pred action)
        (if (eq action 'metadata)
            `(metadata
              (group-function . ,group-function)
              (affixation-function . ,affix-function))
          (complete-with-action action cands str pred)))
      nil t))))

;;;###autoload
(defun flymake-rest-change-checker (checkers)
  "Interactively select and enable/disable checker for the current `major-mode'.
With ARG select a checker regardless of `major-mode'."
  (interactive (list (flymake-rest-change-checker--read-checkers current-prefix-arg)))
  (when checkers
    (dolist (checker checkers)
      (cl-destructuring-bind (_cand _mode checker exists) checker
        ;; Flymake doesn't let us remove a backend once we've added it, simply
        ;; disable it.
        (if (member checker flymake-diagnostic-functions)
            (if exists ;; not disabled
                ;; WARN: For some reason disabling the backend doesn't clear any
                ;; existing reports for it.
                (flymake--disable-backend checker "User chose to disable it")
              (flymake--run-backend checker))
          (add-hook 'flymake-diagnostic-functions checker nil t)))
      (when (called-interactively-p 'interactive)
        (flymake-start)))))

(provide 'flymake-rest-commands)

;;; flymake-rest-commands.el ends here
