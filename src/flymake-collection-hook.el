;;; flymake-collection-hook.el --- Support for binding flymake backends to specific modes -*- lexical-binding: t -*-

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

;; This file implements a way to associate major-modes with flymake checkers.
;; This can be used to automatically enable certain checkers on certain modes.

;;; Code:

(require 'cl-lib)

(define-obsolete-variable-alias 'flymake-rest-config 'flymake-collection-config "2.0.0")
(define-obsolete-variable-alias 'flymake-rest-config-inherit 'flymake-collection-config-inherit "2.0.0")
(define-obsolete-variable-alias 'flymake-rest-hook-ignore-modes 'flymake-collection-hook-ignore-modes "2.0.0")
(define-obsolete-function-alias 'flymake-collection-configured-checkers 'flymake-collection-hook-checkers "2.0.1")
;;;###autoload
(define-obsolete-variable-alias 'flymake-collection-config 'flymake-collection-hook-config "2.0.2")
(define-obsolete-variable-alias 'flymake-collection-config-inherit 'flymake-collection-hook-inherit-config "2.0.2")

;;;###autoload
(defcustom flymake-collection-hook-config
  '(((python-mode python-ts-mode) .
     (flymake-collection-pycodestyle
      (flymake-mypy :disabled t)
      (flymake-collection-pylint :disabled t)
      (flymake-collection-flake8 :disabled t)
      (flymake-collection-ruff :disabled t)))
    (awk-mode . (flymake-collection-awk-gawk))
    ((c-mode c-ts-mode) .
     (flymake-collection-clang
      (flymake-collection-gcc :disabled t)))
    ((c++-mode c++-ts-mode) .
     (flymake-collection-clang
      (flymake-collection-gcc :disabled t)))
    (haskell-mode . (flymake-collection-hlint))
    ((js-mode js2-mode typescript-mode typescript-ts-mode) .
     (flymake-collection-eslint))
    ((json-mode json-ts-mode) .
     (flymake-collection-jq
      (flymake-collection-jsonlint :disabled t)))
    (less-mode flymake-collection-less)
    (markdown-mode
     flymake-collection-markdownlint
     flymake-collection-proselint)
    (lua-mode
     flymake-collection-luacheck
     (flymake-collection-lua :disabled t))
    (nix-mode
     flymake-collection-statix)
    (sql-mode
     flymake-collection-sql-lint
     (flymake-collection-sqlint :disabled t))
    ((ruby-mode ruby-ts-mode) .
     (flymake-collection-rubocop))
    ;; (hledger-mode flymake-collection-hledger)
    ((sh-mode bash-ts-mode) .
     (flymake-collection-shellcheck))
    ((yaml-mode yaml-ts-mode) .
     flymake-collection-yamllint)
    ((web-mode html-ts-mode) .
     (flymake-collection-html-tidy))
    (org-mode flymake-collection-proselint)
    (notmuch-message-mode flymake-collection-proselint)
    (nxml-mode flymake-collection-xmllint))
  "Configuration mapping major-modes to `flymake' backends."
  :type '(alist
          :key-type
          (choice
           (symbol :tag "Mode")
           (repeat (symbol :tag "Mode")))
          :value-type
          (repeat
           :tag "Backends"
           (choice
            (symbol :tag "Backend")
            (cons :tag "Backend with properties"
                  (symbol :tag "Backend")
                  (choice
                   (number :tag "Depth")
                   (plist :tag "Properties"
                          :options ((:disabled boolean)
                                    (:depth integer)
                                    (:predicate function))))))))
  :group 'flymake-collection)

(defcustom flymake-collection-hook-inherit-config nil
  "When true diagnostic hooks inherit parent-mode hooks."
  :type 'boolean
  :group 'flymake-collection)

(defun flymake-collection-hook--configured-checkers-for-mode (mode)
  "Return all checkers configured for MODE in `flymake-collection-hook-config'."
  (cl-dolist (it flymake-collection-hook-config)
    (let ((it-mode (car it))
          (it-conf (cdr it)))
      (cond ((symbolp it-mode)
             (when (equal it-mode mode)
               (cl-return it-conf)))
            ((consp it)
             (when (member mode it-mode)
               (cl-return it-conf)))
            (t
             (user-error
              "Unknown hook predicate=%s in `flymake-collection-hook-config'"
              it))))))

(defun flymake-collection-hook--resolve-configured-checkers (checkers)
  "Resolve all the checkers in CHECKERS.
Resolving converts each checker in CHECKERS, which should be the value-type in
`flymake-collection-hook-config', into a list of (checker . depth) values. This
function will also remove any disabled checkers or checkers with predicates
that are not true."
  (cl-loop
   for conf in checkers
   with predicated-result = nil

   if (symbolp conf)
     collect (cons conf nil)
   else if (consp conf)
     if (numberp (cdr conf))
       collect conf
     else
       do (cl-destructuring-bind (checker &optional &key depth predicate disabled &allow-other-keys)
              conf
            (when (and (not disabled)
                       (or (not predicate)
                           (funcall predicate)))
              (setq predicated-result (cons checker depth))))
       and if predicated-result
         collect predicated-result))

(defun flymake-collection-hook-checkers (mode)
  "Fetch the list of diagnostic functions for MODE as (checker . depth)."
  (let (checkers
        (modes (list mode)))
    ;; Consider all the parent modes as well.
    (when flymake-collection-hook-inherit-config
      (while (setq mode (get mode 'derived-mode-parent))
        (push mode modes)))
    ;; For each mode populate the checkers alist with (checker . depth).
    (dolist (mode modes)
      (setq checkers (append
                      checkers
                      (flymake-collection-hook--resolve-configured-checkers
                       (flymake-collection-hook--configured-checkers-for-mode
                        mode)))))
    checkers))



(defcustom flymake-collection-hook-ignore-modes nil
  "List of modes in which `flymake-collection-hook' is inhibited."
  :type '(repeat :tag "Modes" (symbol :tag "Mode"))
  :group 'flymake-collection)

(defun flymake-collection-hook-set-backends ()
  "Setup `flymake-diagnostic-functions' using `flymake-collection-config'."
  (unless (cl-find-if (lambda (mode)
                        (or (eq major-mode mode)
                            (and (boundp mode)
                                 (eval mode))))
                      flymake-collection-hook-ignore-modes)
    (dolist (it (flymake-collection-hook-checkers major-mode))
      (add-hook 'flymake-diagnostic-functions (car it) (cdr it) t))))

;;;###autoload
(define-obsolete-function-alias 'flymake-rest-hook-setup 'flymake-collection-hook-setup "2.0.0")
;;;###autoload
(define-obsolete-function-alias 'flymake-rest-hook-teardown 'flymake-collection-hook-teardown "2.0.0")

;;;###autoload
(defun flymake-collection-hook-setup ()
  "Setup flymake-hook."
  (add-hook 'after-change-major-mode-hook #'flymake-collection-hook-set-backends))

(defun flymake-collection-hook-teardown ()
  "Tear down flymake-hook."
  (remove-hook 'after-change-major-mode-hook #'flymake-collection-hook-set-backends))


;;; `use-package' integration

;;;###autoload
(with-eval-after-load 'use-package-core
  (defvar flymake-collection-hook-config)

  (declare-function use-package-concat "use-package-core")
  (declare-function use-package-process-keywords "use-package-core")
  (defvar use-package-keywords)
  (defvar use-package-deferring-keywords)

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
                collect `(push (quote ,it) flymake-collection-hook-config))
       body))))

(provide 'flymake-collection-hook)

;;; flymake-collection-hook.el ends here
