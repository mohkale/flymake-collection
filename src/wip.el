;;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defvar-local flymake-collection-define--procs nil
  "The local plist of checker processes running in the current buffer.
When a checker process is begun its pushed into this plist and when its
finished its removed and killed. In the very often circumstance where a
new check is begun while an old check is still pending, the old check is
killed and replaced with the new check.")

(cl-defun flymake-collection--run-checker
    (name
     report-fn
     &key
     command
     parser
     (title nil)
     (write-type 'pipe)
     (temp-file-prefix nil)
     (source-inplace nil))
  "TODO"
  (declare (indent defun) (doc-string 2))

  (unless lexical-binding
    (error "Flymake collection needs lexical-binding to run checker=%s" name))
  (unless (memq write-type '(file pipe))
    (error "Invalid `:write-type' value `%s'" write-type))

  (let (;; Original buffer that is being checked.
        (flymake-collection-source (current-buffer))
        ;; Hook variable. Place any cleanup logic as functions in here.
        flymake-collection-cleanup-hook
        ;; Variable for the checker sub-process.
        flymake-collection-proc
        ;; List of diagnostics parsed for this checker.
        flymake-collection-diagnostics
        ;; Optional variables pointing to a temporary file and directory.
        ;; These will only have values when write-type is 'file. When this
        ;; is the case and source-inplace is t then the temp-directory
        ;; variable will be set. Otherwise the temp file variable will be
        ;; set. In either case these temporary locations needed to be
        ;; cleaned before the checker exits.
        (flymake-collection-temp-dir nil)
        (flymake-collection-temp-file nil))

    ;; Checker has to run against a temporary file create it and update the
    ;; cleanup hook to later delete it.
    (when (eq write-type 'file)
      (let (made-temp-dir)
        (setq
         temporary-file-directory
         (or
          (when source-inplace
            (when-let ((dir (or (when-let ((file (buffer-file-name)))
                                  (file-name-directory file))
                                default-directory)))
              (unless (file-exists-p dir)
                (error "Checker needs to be run in the current-working-directory, \
but that directory does not exist: %s" dir))
              dir))
          (and (setq made-temp-dir t)
               (make-temp-file "flymake-" t))))

        (let ((temporary-file-directory flymake-collection-temp-dir)
              (basename (file-name-nondirectory (or (buffer-file-name)
                                                    (buffer-name)))))
          (setq flymake-collection-temp-file
                (make-temp-file temp-file-prefix nil (concat "_" basename))))

        (if made-temp-dir
            (push (apply-partially #'delete-directory temporary-file-directory t)
                  flymake-collection-cleanup-hook)
          (push (apply-partially #'delete-file flymake-collection-temp-file t)
                flymake-collection-cleanup-hook))))

    ;; Expand command list and then validate it.
    (setq command
          (eval `(list ,@command)
                `((flymake-collection-temp-file . ,flymake-collection-temp-file)
                  (flymake-collection-temp-dir . ,flymake-collection-temp-dir))))
    (unless (seq-every-p #'stringp command)
      (flymake-log :error "Command for checker=%s is invalid %S" name command)
      (mapc #'funcall flymake-collection-cleanup-hook)
      (error "Invalid command for checker=%s" command))

    ;; Kill any running (obsolete) checker processes for the current buffer.
    (when-let ((old-proc (plist-get flymake-collection-define--procs name)))
      (when (process-live-p old-proc)
        (flymake-log :debug "Killing earlier checker process %s" old-proc)
        (kill-process old-proc)))

    ;; Kick-start checker process.
    (save-restriction
      (widen)
      (when flymake-collection-temp-file
        (write-region nil nil flymake-collection-temp-file nil 'silent))
      (setq
       flymake-collection-proc
       (make-process
        :name (concat (symbol-name name) "-flymake")
        :noquery t
        :connection-type 'pipe
        :buffer (generate-new-buffer (concat " *" (symbol-name name) "-flymake*"))
        :command
        (prog1 command
          (flymake-log :debug "Checker command is %s" command))
        :sentinel
        (lambda (flymake-collection-proc _event)
          (cl-block nil
            (when (process-live-p flymake-collection-proc)
              (cl-return))

            ;; Process complete. Parse and report diagnostics, then cleanup.
            (unwind-protect
                (progn
                  ;; If process is obsolete, cancel the result reporting early.
                  (when TODO-obsolete
                    (flymake-log :warning "Canceling obsolete check for %s"
                                 flymake-collection-proc)
                    (cl-return))

                  ;; TODO Parse diagnostics
                  (let (diagnostics)
                    (condition-case-unless-debug error
                        (setq diagnostics
                              (funcall parser (process-buffer flymake-collection-proc)))
                      (error (flymake-log
                              :error "Error while parsing diagnostics for checker=%s: %s"
                              name error))
                      (cl-return))

                    (with-current-buffer flymake-collection-source
                      (save-restriction
                        (widen)
                        (dolist (diagnostic diagnostics)
                          (let ((diag-beg (nth 1 diagnostic))
                                (diag-end (nth 2 diagnostic))
                                (diag-type (nth 3 diagnostic)))
                            (if (and (integer-or-marker-p diag-beg)
                                     (integer-or-marker-p diag-end))
                                (when diag-type
                                  ;; Include the checker title in the message.
                                  (when title
                                    (setf (nth 4 diagnostic)
                                          (concat
                                           (nth 4 diagnostic)
                                           " ("
                                           (propertize title 'face 'flymake-collection-checker)
                                           ")")))

                                  (push (apply #'flymake-make-diagnostic
                                               diagnostic)
                                        flymake-collection-diagnostics))

                              (flymake-log
                               :error
                               "Got invalid buffer position %s or %s in %s"
                               diag-beg diag-end flymake-collection-proc)))))))

                  ;; If process is obsolete after parsing, cancel the result
                  ;; reporting early.
                  (when TODO-obsolete
                    (flymake-log :warning "Canceling obsolete check after parsing for %s"
                                 flymake-collection-proc)
                    (cl-return))

                  ;; Log a warning if the checker process failed, but no checks were
                  ;; parsed.
                  (let ((status (process-exit-status flymake-collection-proc)))
                    (when (and (eq (length flymake-collection-diagnostics) 0)
                               (not (eq status 0)))
                      (flymake-log
                       :warning
                       "Checker gave no diagnostics but had a non-zero exit status %d"
                       status)
                      (flymake-log
                       :debug "Checker output: %s"
                       (with-current-buffer (process-buffer flymake-collection-proc)
                         (buffer-substring-no-properties (point-min) (point-max))))))
                  ;; Report parsed diagnostics.
                  (funcall report-fn flymake-collection-diagnostics))
              ;; Perform cleanup actions.
              (mapc #'funcall flymake-collection-cleanup-hook)
              (when (eq (plist-get flymake-collection-define--procs name)
                        flymake-collection-proc)
                (cl-remf flymake-collection-define--procs name))
              (kill-buffer (process-buffer flymake-collection-proc)))))))

      ;; Push the new-process to to the process alist.
      (setq flymake-collection-define--procs
            (plist-put flymake-collection-define--procs
                       name flymake-collection-proc))
      ;; If checker uses a pipe, send stdin to the process.
      (when (eq write-type 'pipe)
        (when (process-live-p flymake-collection-proc)
          (condition-case-unless-debug error
              (progn
                (process-send-region flymake-collection-proc
                                     (point-min) (point-max))
                (process-send-eof flymake-collection-proc))
            (error (flymake-log :error "Could not send stdin to checker %s" error))
            ))))))

(provide 'wip)
