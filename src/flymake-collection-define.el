;;; flymake-collection-define.el --- A macro to simplify checker creation -*- lexical-binding: t -*-

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

;; This file provides a macro `flymake-collection-define', adapted heavily from
;; [[https://github.com/karlotness/flymake-quickdef/blob/150c5839768a3d32f988f9dc08052978a68f2ad7/flymake-quickdef.el][flymake-quickdef]], to allow streamlined syntax-checker definitions. The
;; intended purpose is to abstract the process creation, management and cleanup
;; for a checker as much as possible, leaving the developer to only have to
;; specify what command to run and how to parse its output.

;; Also in this file you'll find helper macros to parse diagnostics using
;; regexps and simplify JSON processing.
;;
;; `flymake-collection-define-rx' works by defining some regular expressions (one for
;; each severity level of the checker) and then matching each line of the output
;; to a regular expression. Special capture groups have been setup by the parser
;; that should be used by any calling checkers to ensure the correct fields from
;; the output can be parsed. The approach for this was heavily inspired by
;; flychecks :error-parsers feature.
;;
;; `flymake-collection-define' can be used to parse JSON output from the checker into
;; flymake diagnostics. This works by parsing the entire JSON input into a list
;; of diagnostic related data, and then iteratively parsing it into diagnostics.

;;; Code:

(require 'cl-lib)
(require 'flymake)

;;;###autoload
(defvar-local flymake-collection-define--procs nil
  "The local plist of checker processes running in the current buffer.
When a checker process is begun its pushed into this plist and when its
finished its removed and killed. In the very often circumstance where a
new check is begun while an old check is still pending, the old check is
killed and replaced with the new check.")


;;; `flymake-collection-define'

(define-obsolete-function-alias 'flymake-rest-define 'flymake-collection-define "2.0.0")

(defun flymake-collection-define--temp-file
    (temp-dir temp-file source-inplace temp-file-prefix)
  "Let forms for defining a temporary directory and file.
TEMP-DIR and TEMP-FILE are the symbols used for the corresponding variables.
SOURCE-INPLACE specifies whether the TEMP-DIR should be in the same working
directory as the current buffer. Temporary files are named by concatenating
TEMP-FILE-PREFIX with the current buffer file name."
  `((,temp-dir
     ,@(let ((forms
              (append
               (when source-inplace
                 '((when-let ((dir (or (when-let ((file (buffer-file-name)))
                                         (file-name-directory file))
                                       default-directory)))
                     (unless (file-exists-p dir)
                       (error "Checker needs to be run in the cwd, but the cwd \
doesn't exist: %s" dir))
                     dir)))
               '((make-temp-file "flymake-" t)))))
         (if (> (length forms) 1)
             `((or ,@forms))
           forms)))
    (,temp-file
     (let ((temporary-file-directory ,temp-dir)
           (basename (file-name-nondirectory (or (buffer-file-name)
                                                 (buffer-name)))))
       (make-temp-file ,temp-file-prefix nil (concat "_" basename))))))

(defmacro flymake-collection-define--parse-diags
    (title proc-symb diags-symb current-diag-symb source-symb error-parser)
  "Helper macro to parse diagnostics into DIAGS-SYMB.
TITLE is the title of the current syntax checker. PROC-SYMB, DIAGS-SYMB,
CURRENT-DIAGS-SYMB, SOURCE-SYMB, ERROR-PARSER are all described in
`flymake-collection-define'."
  `(with-current-buffer ,source-symb
     (save-restriction
       (widen)
       (with-current-buffer (process-buffer ,proc-symb)
         (goto-char (point-min))
         (save-match-data
           (while (setq ,current-diag-symb (progn ,error-parser))
             (let* ((diag-beg (nth 1 ,current-diag-symb))
                    (diag-end (nth 2 ,current-diag-symb))
                    (diag-type (nth 3 ,current-diag-symb)))
               (if (and (integer-or-marker-p diag-beg)
                        (integer-or-marker-p diag-end))
                   ;; Skip any diagnostics with a type of nil. This makes it
                   ;; easier to filter some out.
                   (when diag-type
                     ;; Include the checker title in the message.
                     ,@(when title
                         `((setf (nth 4 ,current-diag-symb)
                                 (concat
                                  (nth 4 ,current-diag-symb)
                                  ,(concat
                                    " ("
                                    (propertize title 'face 'flymake-collection-checker)
                                    ")")))))
                     (push (apply #'flymake-make-diagnostic
                                  ,current-diag-symb)
                           ,diags-symb))
                 (with-current-buffer ,source-symb
                   (flymake-log
                    :error
                    "Got invalid buffer position %s or %s in %s"
                    diag-beg diag-end ,proc-symb))))))))
     (setq ,diags-symb (nreverse ,diags-symb))))

;; WARN: I can't seem to make docstring optional and use keys, because
;; the key for the first keyword argument will become the docstring if
;; there's no docstring.
(cl-defmacro flymake-collection-define
    (name docstring
          &optional &key title command error-parser write-type
          source-inplace pre-let pre-check (temp-file-prefix ".flymake_"))
  "Quickly define a backend function for use with Flymake.
Define a function NAME which is suitable for use with the variable
`flymake-diagnostic-functions'. DOCSTRING if given will become the
docstring of the checker function.

Available Variables
-------------------
Within the body of NAME several macro specific variables will be
made available for use with ERROR-PARSER or COMMAND, and other
optional arguments such as PRE-LET. This includes:
* flymake-collection-source
  The the buffer where the syntax check originally began.
* flymake-collection-temp-file
  A temporary file where the contents of the current buffer were
  written (only if WRITE-TYPE is \\='file)
* flymake-collection-temp-dir
  The dirname of flymake-collection-temp-file.

Body Execution
--------------
The overall execution of the generated function NAME first makes use
of (1) WRITE-TYPE, (2) SOURCE-INPLACE, (3) PRE-LET, and (4) PRE-CHECK.
Then a process is created using (4) COMMAND. Once the process finishes
ERROR-PARSER is called (until it returns nil) to get the next
diagnostic which is then provided to `flymake'. TITLE if provided is
used to suffix the message for each diagnostic.

WRITE-TYPE specifies how the process for a syntax check should recieve
the input. It should one of \\='pipe or \\='file (defaulting to \\='pipe).
When set to \\='file a temporary file will ve created, copying the contents
of the `current-buffer'. The variable flymake-collection-temp-file and
flymake-collection-temp-dir will be bound in the body of NAME and provide
access to this temp-file.
When set to \\='pipe, all of the `current-buffer' will be passed to the
process on its standard input stream after it has begun.

SOURCE-INPLACE determines whether to also create a temporary directory
for a temporary file (when using a WRITE-TYPE of \\='file) or whether to
place the temporary file in the same directory as the file being checked.
This can be useful if the syntax checker also resolves imports or packages
and thus needs to be in the same directory. This is disabled by default
meaning the file is placed in folder in the systems temporary directory.

PRE-LET is a `let*' form that is assigned after any checker agnostic
variables. Place anything you want exposed to everything else in the
checker here.

PRE-CHECK is a Lisp form that will be executed immeadiately before any
pending checker processes are killed and a new process is begun. It can
check conditions to ensure launching the checker program is possible. If
something is wrong it should signal an error.

COMMAND is a Lisp form which evaluates to a list of strings that will be
used to start the checker process. It should be suitable for use as the
:command argument to the `make-process' function.

ERROR-PARSER is a lisp-form that should, each time it is evaluated,
return the next diagnostic from the checker output. The result should be
a value that can be passed to the `flymake-make-diagnostic' function. Once
there are no more diagnostics to parse this form should evaluate to nil.

TEMP-FILE-PREFIX overrides the prefix of temporary file names created by
the checker. This is useful for checker programs that have issues running
on hidden files."
  (declare (indent defun) (doc-string 2))
  (unless lexical-binding
    (error "Need lexical-binding for flymake-collection-define (%s)" name))
  (dolist (elem (list (cons 'command command)
                      (cons 'error-parser error-parser)))
    (unless (cdr elem)
      (error "Missing flymake backend definition `%s'" (car elem))))

  (setq write-type (or (eval write-type) 'pipe))
  (setq source-inplace (eval source-inplace))

  (unless (memq write-type '(file pipe))
    (error "Invalid `:write-type' value `%s'" write-type))

  (let* ((temp-dir-symb 'flymake-collection-temp-dir)
         (temp-file-symb 'flymake-collection-temp-file)
         (proc-symb 'proc)
         (err-symb 'flymake-collection-err)
         (source-symb 'flymake-collection-source)
         (diags-symb 'diags)
         (current-diag-symb 'diag)
         (cleanup-form (when (eq write-type 'file)
                         (if source-inplace
                             `((delete-file ,temp-file-symb))
                           `((delete-directory ,temp-dir-symb t)))))
         (not-obsolete-form
          `((eq ,proc-symb
                (plist-get (buffer-local-value 'flymake-collection-define--procs
                                               ,source-symb)
                           ',name)))))
    `(defun ,name (report-fn &rest _args)
       ,docstring
       (let* ((,source-symb (current-buffer))
              ,@(when (eq write-type 'file)
                  (flymake-collection-define--temp-file
                   temp-dir-symb temp-file-symb source-inplace temp-file-prefix))
              ,@pre-let)
         ;; With vars defined, do pre-check.
         ,@(when pre-check
             `((condition-case ,err-symb
                   ,pre-check
                 (error ,@cleanup-form
                        (signal (car ,err-symb) (cdr ,err-symb))))))
         ;; Kill any running (obsolete) checkers for current checker and buffer.
         (let ((,proc-symb (plist-get flymake-collection-define--procs ',name)))
           (when (process-live-p ,proc-symb)
             (flymake-log :debug "Killing earlier checker process %s" ,proc-symb)
             (kill-process ,proc-symb)))
         ;; Kick-start checker process.
         (save-restriction
           (widen)
           ,@(when (eq write-type 'file)
               `((write-region nil nil ,temp-file-symb nil 'silent)))
           (let (,proc-symb)
             (setq
              ,proc-symb
              (make-process
               :name ,(concat (symbol-name name) "-flymake")
               :noquery t
               :connection-type 'pipe
               :buffer (generate-new-buffer
                        ,(concat " *" (symbol-name name) "-flymake*"))
               :command
               (prog1 ,command
                 (flymake-log :debug "Checker command is %s" ,command))
               :sentinel
               (lambda (,proc-symb _event)
                 (unless (process-live-p ,proc-symb)
                   (unwind-protect
                       (if ,@not-obsolete-form
                           (let ((,diags-symb nil) ,current-diag-symb)
                             (flymake-collection-define--parse-diags
                              ,title
                              ,proc-symb
                              ,diags-symb
                              ,current-diag-symb
                              ,source-symb
                              ,error-parser)
                             ;; Report diagnostics when still not-obsolete.
                             (if ,@not-obsolete-form
                                 (progn
                                   (let ((status (process-exit-status ,proc-symb)))
                                     (when (and (eq (length ,diags-symb) 0)
                                                (not (eq status 0)))
                                       (flymake-log
                                        :warning
                                        "Checker gave no diagnostics but had a non-zero \
exit status %d\nStderr: %s"
                                        status
                                        (with-current-buffer (process-buffer ,proc-symb)
                                          (format "%s" (buffer-substring-no-properties
                                                        (point-min) (point-max)))))))
                                   (funcall report-fn ,diags-symb))
                               ;; In case the check was cancelled after processing began
                               ;; but before it finished.
                               (flymake-log :warning "Canceling obsolete check %s" ,proc-symb)))
                         (flymake-log :warning "Canceling obsolete check %s" ,proc-symb))
                     ;; Finished linting, cleanup any temp-files and then kill
                     ;; the process buffer.
                     ,@cleanup-form
                     (when (eq (plist-get flymake-collection-define--procs ',name)
                               ,proc-symb)
                       (cl-remf flymake-collection-define--procs ',name))
                     (kill-buffer (process-buffer ,proc-symb)))))))
             ;; Push the new-process to the process to the process alist.
             (setq flymake-collection-define--procs
                   (plist-put flymake-collection-define--procs ',name ,proc-symb))
             ;; If piping, send data to the process.
             ,@(when (eq write-type 'pipe)
                 `((when (process-live-p ,proc-symb)
                     (condition-case-unless-debug error
                         (progn
                           (process-send-region ,proc-symb (point-min) (point-max))
                           (process-send-eof ,proc-symb))
                       (error (flymake-log :error "Could not send stdin to checker %s" error))))))
             ;; Return value of syntax-checker is checker function.
             ,proc-symb))))))


;;; `flymake-collection-define-rx'

(eval-when-compile
  (require 'rx))

(define-obsolete-variable-alias 'flymake-rest-define-parse-rx-constituents 'flymake-collection-define-parse-rx-constituents "2.0.0")
(define-obsolete-function-alias 'flymake-rest-define-rx 'flymake-collection-define-rx "2.0.0")

(defconst flymake-collection-define-parse-rx-constituents
  `((file-name ,(lambda (body)
                  (rx-to-string
                   `(group-n 1 ,@(or (cdr body)
                                     '((minimal-match
                                        (one-or-more not-newline)))))
                   t))
               0 nil) ;; group 1
    (line . ,(rx (group-n 2 (one-or-more digit))))
    (column . ,(rx (group-n 3 (one-or-more digit))))
    (message ,(lambda (body)
                (rx-to-string
                 `(group-n 4 ,@(or (cdr body)
                                   '((minimal-match
                                      (one-or-more not-newline)))))
                 t))
             0 nil)
    (id ,(lambda (body)
           (rx-to-string `(group-n 5 ,@(cdr body)) t))
        0 nil)
    (end-line . ,(rx (group-n 6 (one-or-more digit))))
    (end-column . ,(rx (group-n 7 (one-or-more digit))))))

(defmacro flymake-collection-define--parse-rx (regexps)
  "`flymake-collection-define' parser using regular expressions.

This macro generates a parser that for each line of output from the
checker process, matches one or more regular expressions and then
converts the result to a valid flymake diagnostic that can be
passed back to `flymake-make-diagnostic'.

REGEXPS should be an alist with the car of each entry being the
severity of the diagnostic it matches (as a symbol that will be
turned into a keyword by this macro) and the cdr should be a
sequence of entries that can be interpreted by the `rx' macro.
To simplify matching specific fields in the parsed output several
helper extensions to `rx' have been defined such as file-name or
line. For a list of these see `flymake-collection-define-parse-rx-constituents'.
The only required fields that MUST be parsed are the line number
and message. If these are ommited the matched diagnostic will be
skipped.

WARN: You should not try to capture any extra fields outside of
the special ones described above. This is because any extra capture
groups are used to associate the severity of the diagnostic to the
regexp that matched it (as a performance improvement).

For an example of this macro in action, see `flymake-collection-pycodestyle'."
  (unless (> (length regexps) 0)
    (error "Must supply at least one regexp for error, warning or note"))

  (let* ((group-count (length flymake-collection-define-parse-rx-constituents))
         (regexps
          ;; To avoid having to rematch each diagnostic more than once we append
          ;; a special extra capture group (greater than all the ones above) that
          ;; simply matches the empty string. Then we can index the groups after
          ;; the ones above and use that to determine the severity of the symbol.
          (cl-loop for (severity . regex) in regexps
                   with count = group-count
                   do (setq count (1+ count))
                   collect (cons `(seq ,@regex (group-n ,count ""))
                                 (intern (concat ":" (symbol-name severity))))))
         (combined-regex
          (let ((rx-constituents (append flymake-collection-define-parse-rx-constituents
                                         (bound-and-true-p rx-constituents) nil)))
            (rx-to-string `(or ,@(mapcar #'car regexps))
                          'no-group)))
         (severity-seq (mapcar #'cdr regexps)))
    ;; Because if this evaluates to nil `flymake-collection-define' thinks there
    ;; are no-more diagnostics to be parsed, we wrap it in a loop that exits
    ;; the moment we find a match, but otherwise keeps moving through diagnostics
    ;; until there actually aren't any more to match.
    `(let (res ; file-name
           line column message id end-line end-column severity-ix)
       (while (and (not res)
                   (search-forward-regexp ,combined-regex nil t))
         (setq
          res
          (save-match-data
            (save-excursion
              (setq ; file-name (match-string 1)
                    line (match-string 2)
                    column (match-string 3)
                    message (match-string 4)
                    id (match-string 5)
                    end-line (match-string 6)
                    end-column (match-string 7)
                    severity-ix (- (seq-find #'match-string
                                             (number-sequence ,(1+ group-count)
                                                              ,(+ group-count (length regexps))))
                                   ,(1+ group-count)))
              (cond
               ;; Log an error when any of the required fields are missing.
               ,@(cl-loop for it in '(severity-ix line message)
                          collect
                          `((not ,it)
                            (flymake-log :error
                                         ,(format
                                           "Matched diagnostic didn't capture a %s group"
                                           (symbol-name it)))
                            nil))
               (t
                (let ((loc (flymake-diag-region flymake-collection-source
                                                (string-to-number line)
                                                (when column
                                                  (string-to-number column))))
                      (loc-end (when end-line
                                 (flymake-diag-region flymake-collection-source
                                                      (string-to-number end-line)
                                                      (when end-column
                                                        (string-to-number end-column))))))
                  (when loc-end
                    (setcdr loc (cdr loc-end)))
                  (list flymake-collection-source
                        (car loc)
                        (cdr loc)
                        (nth severity-ix (quote ,severity-seq))
                        (concat
                         (when id
                           (concat (propertize id 'face 'flymake-collection-diag-id) " "))
                         message)))))))))
       res)))

(cl-defmacro flymake-collection-define-rx
    (name docstring
          &optional &key title command write-type
          source-inplace pre-let pre-check temp-file-prefix regexps)
  "`flymake-collection-define' helper using `rx' syntax to parse diagnostics.
This helper macro adapts `flymake-collection-define' to use an error-parser
built from a collections of REGEXPS (see `flymake-collection-define--parse-rx').

See `flymake-collection-define' for a description of NAME, DOCSTRING, TITLE,
COMMAND,WRITE-TYPE, SOURCE-INPLACE, PRE-LET, PRE-CHECK, and TEMP-FILE-PREFIX."
  (declare (indent defun) (doc-string 2))
  `(flymake-collection-define ,name
     ,docstring
     :title ,title
     :command ,command
     :write-type ,write-type
     :source-inplace ,source-inplace
     :pre-let ,pre-let
     :pre-check ,pre-check
     :temp-file-prefix ,temp-file-prefix
     :error-parser
     (flymake-collection-define--parse-rx ,regexps)))


;;; `flymake-collection-define-enumerate'

(define-obsolete-function-alias 'flymake-rest-define-enumerate 'flymake-collection-define-enumerate "2.0.0")

(cl-defmacro flymake-collection-define-enumerate
  (name docstring
        &optional &key title command write-type source-inplace
        pre-let pre-check temp-file-prefix generator enumerate-parser)
  "`flymake-collection-define' helper for dealing with serialised diagnostics.
This helper parses a collection of diagnostics using GENERATOR and then
enumerates through it, entry by entry using ENUMERATE-PARSER. This is useful
for linters that produce output such as JSON, to avoid having to reparse the
output again and again.

The value of the current entry from GENERATOR in ENUMERATE-PARSER will be set to
the variable `it'. ENUMERATE-PARSER should evaluate to a form that can be passed
to `flymake-make-diagnostic'.

See `flymake-collection-define' for a description of NAME, DOCSTRING, TITLE,
COMMAND, WRITE-TYPE, SOURCE-INPLACE, PRE-LET, PRE-CHECK, and TEMP-FILE-PREFIX."
  (declare (indent defun) (doc-string 2))
  (let ((entries-var 'flymake-collection-entries)
        (parsed-var 'flymake-collection-parsed))
    `(flymake-collection-define ,name
       ,docstring
       :title ,title
       :command ,command
       :write-type ,write-type
       :source-inplace ,source-inplace
       :pre-let ,(append `((,entries-var)
                           (,parsed-var))
                         pre-let)
       :pre-check ,pre-check
       :temp-file-prefix ,temp-file-prefix
       :error-parser
       (progn
         (unless ,parsed-var
           (setq ,entries-var ,generator
                 ,parsed-var t))
         (let (it res)
           ;; While we haven't found a new diagnostic to return, BUT there're
           ;; still diagnostics that can be found in the parsed checker output.
           (while (and (not res)
                       (setq it (pop ,entries-var)))
             (setq res ,enumerate-parser))
           res)))))

(provide 'flymake-collection-define)

;;; flymake-collection-define.el ends here
