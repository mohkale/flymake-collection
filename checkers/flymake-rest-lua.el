;;; flymake-rest-lua.el --- lua diagnostic function using the lua compiler -*- lexical-binding: t -*-

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

;;; Code:

(require 'flymake)
(require 'flymake-rest)

(eval-when-compile
  (require 'flymake-rest-define)
  (require 'flymake-rest-parse-rx))

;;;###autoload (autoload 'flymake-rest-lua "flymake-rest-lua")
(flymake-rest-define flymake-rest-lua
  "A Lua syntax checker using the Lua compiler.

See URL `http://www.lua.org/'."
  :pre-let ((lua-exec (executable-find "luac")))
  :pre-check (unless lua-exec
               (user-error "Cannot find lua compiler executable"))
  :write-type 'pipe
  :command `(,lua-exec "-p" "-")
  :error-parser
  (flymake-rest-parse-rx
   ((error bol
           ;; Skip the name of the luac executable.
           (minimal-match (zero-or-more not-newline))
           ": stdin:" line ": " (message) eol))))

(provide 'flymake-rest-lua)

;;; flymake-rest-lua.el ends here
