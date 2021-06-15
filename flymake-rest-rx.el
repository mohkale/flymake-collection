;;; flymake-rest-rest.el --- A macro to simplify checker creation -*- lexical-binding: t -*-

;;; Commentary:
;; TODO
;; TODO: license

;;; Code:

(defconst flymake-rest-rx-constituents
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

(defmacro flymake-rest-parse-rx (regexps)
  "Helper for `flymake-rest-define' which tries to emulate flychecks
:error-parsers.

This macro generates a parser that for each line of output from the
checker process, runs one or more REGEXPs on it and then converts the
result to a valid flymake diagnostic (that can be passed back to
`flymake-make-diagnostic').

TODO: describe arguments.
"
  (unless (> (length regexps) 0)
    (error "Must supply at least one regexp for error, warning or note"))
  ;; To avoid having to rematch each diagnostic more than once we append
  ;; a special extra capture group (greater than all the ones above) that
  ;; simply matches the empty string. Then we can index the groups after
  ;; 7 and use that to determine the severity of the symbol.
  (setq regexps
        (cl-loop for (severity . regex) in regexps
                 with count = 7
                 do (setq count (1+ count))
                 collect (cons `(seq ,@regex (group-n ,count ""))
                               (intern (concat ":" (symbol-name severity))))))

  (let ((combined-regex
         (let ((rx-constituents (append flymake-rest-rx-constituents
                                        rx-constituents nil)))
           (rx-to-string `(or ,@(mapcar #'car regexps))
                         'no-group)))
        (severity-seq (mapcar #'cdr regexps)))
    ;; Because if this evaluates to nil `flymake-rest-define' thinks there
    ;; are no-more diagnostics to be parsed, we wrap it in a loop that exits
    ;; the moment we find a match, but otherwise keeps moving through diagnostics
    ;; until there actually aren't any more to match.
    `(let (res
           file-name line column message id end-line end-column severity-ix)
       (while (and (not res)
                   (search-forward-regexp ,combined-regex nil t))
         (setq
          res
          (save-match-data
            (save-excursion
              (setq file-name (match-string 1)
                    line (match-string 2)
                    column (match-string 3)
                    message (match-string 4)
                    id (match-string 5)
                    end-line (match-string 6)
                    end-column (match-string 7)
                    severity-ix (seq-find #'match-string
                                          (number-sequence 0 ,(- (length regexps) 1))))
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
                (let ((loc (flymake-diag-region fmqd-source
                                                (string-to-number line)
                                                (when column
                                                  (string-to-number column))))
                      (loc-end (when end-line
                                 (flymake-diag-region fmqd-source
                                                      (string-to-number end-line)
                                                      (when end-column
                                                        (string-to-number end-column))))))
                  (when loc-end
                    (setcdr loc (cdr loc-end)))
                  (list fmqd-source
                        (car loc)
                        (cdr loc)
                        (nth severity-ix (quote ,severity-seq))
                        (concat
                         (when id
                           (concat (propertize id 'face 'flymake-diag-id!) " "))
                         message)))))))))
       res)))

(provide 'flymake-rest-rx)
