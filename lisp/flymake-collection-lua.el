;;; flymake-collection-lua.el --- lua diagnostic function using the lua compiler -*- lexical-binding: t -*-

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

;; `flymake' syntax checker for lua using the lua compiler.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

;;;###autoload (autoload 'flymake-collection-lua "flymake-collection-lua")
(flymake-collection-define-rx flymake-collection-lua
  "A Lua syntax checker using the Lua compiler.

See URL `http://www.lua.org/'."
  :pre-let ((lua-exec (executable-find "luac")))
  :pre-check (unless lua-exec
               (user-error "Cannot find lua compiler executable"))
  :write-type 'pipe
  :command `(,lua-exec "-p" "-")
  :regexps
  ((error bol
          ;; Skip the name of the luac executable.
          (minimal-match (zero-or-more not-newline))
          ": stdin:" line ": " (message) eol)))

(provide 'flymake-collection-lua)

;;; flymake-collection-lua.el ends here
