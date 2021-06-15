;;; flymake-rest-hook.el --- Support for binding flymake backends to specific modes -*- lexical-binding: t -*-

;; Copyright (C) 2021 Mohsin Kaleem

;;;###autoload
(defcustom flymake-rest-config nil
  "Configuration mapping major-modes to flymake-backends."
  :type 'list)

(defcustom flymake-rest-config-inherit nil
  "When true any configured checkers for a parent major-mode are
also added to `flymake-diagnostic-functions'."
  :type 'boolean)

(defun flymake-rest-configured-checkers (mode)
  (let (checkers
        (modes (list mode)))
    ;; Consider all the parent modes as well.
    (when flymake-rest-config-inherit
      (while (setq mode (get mode 'derived-mode-parent))
        (push mode modes)))
    ;; For each mode populate the checkers alist with (checker . depth).
    (dolist (mode modes)
      (dolist (conf (alist-get mode flymake-rest-config))
        (cond ((symbolp conf)
               (push (cons conf nil) checkers))
              ((consp conf)
               (cl-destructuring-bind (checker &optional &key depth predicate disabled &allow-other-keys)
                   (if (numberp conf)
                       `(,(car conf) :depth ,(cdr conf))
                     conf)
                 (when (and (not disabled)
                            (or (not predicate)
                                (funcall predicate)))
                   (push (cons checker depth) checkers))))
              (t
               (warn "Unknown checker config in `flymake-rest-config': %s" conf)))))
    (nreverse checkers)))

(defun flymake-rest-hook-set-backends ()
  "Function to add all the diagnostic for the current-major mode
from `flymake-rest-config' to `flymake-diagnostic-functions'."
  (dolist (it flymake-rest-configured-checkers)
    (add-hook 'flymake-diagnostic-functions (car it) (cdr it) t)))

(defun flymake-rest-hook-setup ()
  "Setup flymake-hook."
  (add-hook 'after-change-major-mode-hook #'flymake-rest-hook-set-backends))

(defun flymake-rest-hook-teardown ()
  "Tear down flymake-hook."
  (remove-hook 'after-change-major-mode-hook #'flymake-rest-hook-set-backends))

(provide 'flymake-rest-hook)
