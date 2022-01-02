;;; flymake-rest.el --- Core features for flymake-rest -*- lexical-binding: t -*-

;; Copyright (C) 2021 Mohsin Kaleem

;; Author: Mohsin Kaleem <mohkale@kisara.moe>
;; Created: 15 June 2021
;; Homepage: https://github.com/mohkale/flymake-rest
;; Keywords: language tools
;; Package-Requires: ((emacs "27.1") (let-alist "1.0"))
;; SPDX-License-Identifier: MIT
;; Version: 1.0.0

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

;; flymake-rest is a helper for migrating from flycheck to flymake.
;;
;; This includes the definition of several diagnostic functions, hooks
;; to specify the precedence and preferred order of them and the means
;; to easily configure flymake linting.
;;
;; For more see [[file:README.org][README.org]] or https://github.com/mohkale/flymake-rest.

;;; Code:

(defgroup flymake-rest nil
  "Flymake flycheck compatibility."
  :prefix "flymake-rest"
  :group 'flymake)

(defgroup flymake-rest-faces nil
  "Faces used by flymake-rest."
  :group 'flymake-rest
  :group 'faces)

(defface flymake-rest-checker
  '((t (:inherit (dired-directory bold))))
  "Title of a checker as shown in the diagnostic message.")

(defface flymake-rest-diag-id
  '((t (:inherit font-lock-type-face)))
  "Id of a diagnostic.")

(defun flymake-rest-parse-json (output)
  "Helper for `flymake-rest-define' to parse JSON OUTPUT.

Adapted from `flycheck-parse-json'. This reads a bunch of JSON-Lines
like output from OUTPUT into a list and then returns it."
  (let (objects
        (json-array-type 'list)
        (json-false nil))
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

(defcustom flymake-rest-remote-algorithm
  'local
  "How `flymake-rest' should handle remote files and buffers."
  :type '(choice (const :tag "Run the checker on the local machine" local)
                 (const :tag "Run the checker on the remote machine" remote)
                 (const :tag "Disable syntax checks on remote files" cancel)))

(defun flymake-rest-executable-find (command)
  (executable-find command (eq flymake-rest-remote-algorithm 'remote)))

(provide 'flymake-rest)

;;; flymake-rest.el ends here
