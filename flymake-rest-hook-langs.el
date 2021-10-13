;;; flymake-rest-hook-langs.el --- Default mode associations for `flymake-rest-hook' -*- lexical-binding: t -*-

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

;; This file sets the default associations for `flymake-rest-config' for all
;; supported major-modes. This is an opinionated section with certain checkers
;; enabled or disabled based on personal preference, which is why it's not
;; loaded by default. Use this when you don't want to setup mode->checker
;; associations yourself.

;;; Code:

(require 'flymake-rest-hook)

(setq flymake-rest-config
 (append
  '((python-mode
     flymake-rest-pycodestyle
     (flymake-mypy :disabled t)
     (flymake-rest-pylint :disabled t))
    (awk-mode flymake-rest-awk-gawk)
    (c-mode
     flymake-rest-clang
     (flymake-rest-gcc :disabled t))
    (c++-mode
     flymake-rest-clang
     (flymake-rest-gcc :disabled t))
    (js-mode flymake-rest-eslint)
    (js2-mode flymake-rest-eslint)
    (typescript-mode flymake-rest-eslint)
    (json-mode
     flymake-rest-jq
     (flymake-rest-jsonlint :disabled t))
    (less-mode flymake-rest-less)
    (markdown-mode
     flymake-rest-markdownlint
     flymake-rest-proselint)
    (lua-mode
     flymake-rest-luacheck
     (flymake-rest-lua :disabled t))
    (sql-mode
     flymake-rest-sql-lint
     (flymake-rest-sqlint :disabled t))
    (ruby-mode flymake-rest-rubocop)
    ;; (hledger-mode flymake-rest-hledger)
    (sh-mode flymake-rest-shellcheck)
    (yaml-mode flymake-rest-yamllint)
    (web-mode flymake-rest-html-tidy)
    (org-mode flymake-rest-proselint)
    (notmuch-message-mode flymake-rest-proselint)
    (nxml-mode flymake-rest-xmllint))
  flymake-rest-config))

(provide 'flymake-rest-hook-langs)

;;; flymake-rest-hook-langs.el ends here
