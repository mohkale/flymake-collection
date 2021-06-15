;;; flymake-rest-use-package.el --- use-package extensions for flymake-rest -*- lexical-binding: t -*-

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
