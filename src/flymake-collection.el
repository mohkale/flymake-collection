;;; flymake-collection.el --- Collection of checkers for flymake, bringing flymake to the level of flycheck -*- lexical-binding: t -*-

;; Copyright (C) 2021 Mohsin Kaleem

;; Author: Mohsin Kaleem <mohkale@kisara.moe>
;; Created: 15 June 2021
;; Homepage: https://github.com/mohkale/flymake-collection
;; Keywords: language tools
;; Package-Requires: ((emacs "28.1") (let-alist "1.0") (flymake "1.2.1"))
;; SPDX-License-Identifier: MIT
;; Version: 2.0.2

;; Copyright (c) 2021 Mohsin Kaleem
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; flymake-collection is a helper for migrating from flycheck to flymake.
;;
;; This includes the definition of several diagnostic functions, hooks
;; to specify the precedence and preferred order of them and the means
;; to easily configure flymake linting.
;;
;; For more see [[file:README.org][README.org]] or https://github.com/mohkale/flymake-collection.

;;; Code:

(defgroup flymake-collection nil
  "Flymake flycheck compatibility."
  :prefix "flymake-collection"
  :group 'flymake)

(defgroup flymake-collection-faces nil
  "Faces used by flymake-collection."
  :group 'flymake-collection
  :group 'faces)

(defface flymake-collection-checker
  '((t (:inherit (dired-directory bold))))
  "Title of a checker as shown in the diagnostic message.")

(defface flymake-collection-diag-id
  '((t (:inherit font-lock-type-face)))
  "Id of a diagnostic.")

(defun flymake-collection-parse-json (output)
  "Helper for `flymake-collection-define' to parse JSON OUTPUT.

Adapted from `flycheck-parse-json'. This reads a bunch of JSON-Lines
like output from OUTPUT into a list and then returns it."
  (let (objects
        (json-array-type 'list)
        (json-false nil))
    (ignore json-array-type json-false)
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (not (eobp))
        (when (memq (char-after) '(?\{ ?\[))
          (push (json-parse-buffer
                 :object-type 'alist :array-type 'list
                 :null-object nil :false-object nil)
                objects))
        (forward-line)))
    objects))

(define-obsolete-face-alias 'flymake-rest-checker 'flymake-collection-checker "2.0.0")
(define-obsolete-face-alias 'flymake-rest-diag-id 'flymake-collection-diag-id "2.0.0")
(define-obsolete-function-alias 'flymake-rest-parse-json 'flymake-collection-parse-json "2.0.0")

(provide 'flymake-collection)

;;; flymake-collection.el ends here
