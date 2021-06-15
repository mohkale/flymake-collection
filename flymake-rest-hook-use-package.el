;;; flymake-rest-use-package.el --- use-package extensions for flymake-rest -*- lexical-binding: t -*-

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

;;; Commentary
;; This file defines a use-package extension for pushing entries onto
;; `flymake-rest-config'.

;;; Code:

(require 'use-package-core)

(defvar flymake-rest-config) ;; In [[file:flymake-rest-hook.el][flymake-rest-hook.el]].

;; Add to use-package-keywords, just after :custom.
(unless (member :flymake-hook use-package-keywords)
  (let ((tail (nthcdr (cl-position :custom use-package-keywords)
                      use-package-keywords)))
    (setcdr tail (cons :flymake-hook (cdr tail)))))

(defun use-package-normalize/:flymake-hook (_name _keyword args)
  args)

(defun use-package-handler/:flymake-hook (name-symbol _ hooks rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    (use-package-concat
     (cl-loop for it in hooks
              collect `(push (quote ,it) flymake-rest-config))
     body)))

(provide 'flymake-rest-hook-use-package)
