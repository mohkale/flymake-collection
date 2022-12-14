;;; flymake-collection-luacheck.el --- luacheck diagnostic function -*- lexical-binding: t -*-

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

;; `flymake' syntax checker for lua using luacheck.

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

(defcustom flymake-collection-luacheck-standards nil
  "The standards to use in luacheck.

The value of this variable is either a list of strings denoting
the standards to use, or nil to pass nothing to luacheck.  When
non-nil, pass the standards via one or more `--std' options."
  :type '(choice (const :tag "Default" nil)
                 (repeat :tag "Custom standards"
                         (string :tag "Standard name")))
  :group 'flymake-collection)

(defcustom flymake-collection-luacheck-rc nil ; ".luacheckrc"
  "Optional configuration file for luacheck."
  :type '(choice (const :tag "Default" nil)
                 (file :tag "Config file"))
  :group 'flymake-collection)

;;;###autoload (autoload 'flymake-collection-luacheck "flymake-collection-luacheck")
(flymake-collection-define-rx flymake-collection-luacheck
  "A Lua syntax checker using luacheck.

See URL `https://github.com/mpeterv/luacheck'."
  :title "luacheck"
  :pre-let ((luacheck-exec (executable-find "luacheck")))
  :pre-check (unless luacheck-exec
               (error "Cannot find luacheck executable"))
  :write-type 'pipe
  :command `(,luacheck-exec
             "--formatter" "plain"
             "--codes"                   ; Show warning codes
             "--no-color"
             ,@(when flymake-collection-luacheck-standards
                 (list "--std"
                       (mapconcat #'identity flymake-collection-luacheck-standards "+")))
             ,@(when flymake-collection-luacheck-rc
                 (list "--config" flymake-collection-luacheck-rc))
             ,@(when-let ((file (buffer-file-name flymake-collection-source)))
                 (list "--filename" file))
             "-")
  :regexps
  ;; NOTE: `luacheck' before 0.11.0 did not output codes for errors, hence the ID is optional in the error pattern.
  ((warning bol (optional (file-name)) ":" line ":" column ":"           " (" (id "W" (one-or-more digit)) ") "  (message) eol)
   (error   bol (optional (file-name)) ":" line ":" column ":" (optional " (" (id "E" (one-or-more digit)) ") ") (message) eol)))

(provide 'flymake-collection-luacheck)

;;; flymake-collection-luacheck.el ends here
