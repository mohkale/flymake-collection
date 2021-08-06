;;; flymake-rest-rest.el --- A macro to simplify checker creation -*- lexical-binding: t -*-

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
;; to parse plaintext output from the checker into flymake diagnostics. This works
;; by defining some regular expressions (one for each severity level of the checker)
;; and then matching each line of the output to a regular expression. Special
;; capture groups have been setup by the parser that should be used by any calling
;; checkers to ensure the correct fields from the output can be parsed.
;;
;; The approach implemented here was heavily inspired by flychecks :error-parsers
;; feature.

;;; Code:

(defconst flymake-rest-parse-rx-constituents
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

;;;###autoload
(defmacro flymake-rest-parse-rx (regexps)
  "`flymake-rest-define' parser using regular expressions.

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
line. For a list of these see `flymake-rest-parse-rx-constituents'.
The only required fields that MUST be parsed are the line number
and message. If these are ommited the matched diagnostic will be
skipped.

WARN: You should not try to capture any extra fields outside of
the special ones described above. This is because any extra capture
groups are used to associate the severity of the diagnostic to the
regexp that matched it (as a performance improvement).

For an example of this macro in action, see `flymake-rest-pycodestyle'."
  (unless (> (length regexps) 0)
    (error "Must supply at least one regexp for error, warning or note"))

  (let* ((group-count (length flymake-rest-parse-rx-constituents))
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
          (let ((rx-constituents (append flymake-rest-parse-rx-constituents
                                         rx-constituents nil)))
            (rx-to-string `(or ,@(mapcar #'car regexps))
                          'no-group)))
         (severity-seq (mapcar #'cdr regexps)))
    ;; Because if this evaluates to nil `flymake-rest-define' thinks there
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
                (let ((loc (flymake-diag-region flymake-rest-source
                                                (string-to-number line)
                                                (when column
                                                  (string-to-number column))))
                      (loc-end (when end-line
                                 (flymake-diag-region flymake-rest-source
                                                      (string-to-number end-line)
                                                      (when end-column
                                                        (string-to-number end-column))))))
                  (when loc-end
                    (setcdr loc (cdr loc-end)))
                  (list flymake-rest-source
                        (car loc)
                        (cdr loc)
                        (nth severity-ix (quote ,severity-seq))
                        (concat
                         (when id
                           (concat (propertize id 'face 'flymake-rest-diag-id) " "))
                         message)))))))))
       res)))

(provide 'flymake-rest-parse-rx)

;;; flymake-rest-parse-rx.el ends here
