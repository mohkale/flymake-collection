;;; flymake-collection-awk-gawk.el --- awk/gawk diagnostic function -*- lexical-binding: t -*-

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

;; `flymake' syntax checker for awk using GNU awk's built-in lint checker.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

;;;###autoload (autoload 'flymake-collection-awk-gawk "flymake-collection-awk-gawk")
(flymake-collection-define-rx flymake-collection-awk-gawk
  "GNU awk's built-in --lint checker."
  :title "gawk-awk"
  :pre-let ((gawk-exec (executable-find "gawk")))
  :pre-check (unless gawk-exec
               (error "Cannot find gawk executable"))
  :write-type 'pipe
  :command (list gawk-exec
                 ;; Avoid code execution.  See https://github.com/w0rp/ale/pull/1411
                 "--source" "BEGIN{exit} END{exit 0}"
                 "-f" "-"
                 "--lint"
                 null-device)
  :regexps
  ((error   bol (? "g") "awk: -:" line ": " (or "fatal" "error") ": " (message) eol)
   (warning bol (? "g") "awk: -:" line ": " "warning"            ": " (message) eol)))

(provide 'flymake-collection-awk-gawk)

;;; flymake-collection-awk-gawk.el ends here
