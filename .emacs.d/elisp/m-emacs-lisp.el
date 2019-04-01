;;; m-emacs-lisp.el --- Emacs lisp configuration -*- lexical-binding: t -*-

;; Author: Matthew Newton
;; Maintainer: Matthew Newton
;; Version: version
;; Package-Requires: ()
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; My Emacs Lisp configuration

;;; Code:

(use-package lisp-extra-font-lock
  :config
  (lisp-extra-font-lock-global-mode 1))

(use-package lively
  :commands
  (lively-shell-command)
  :bind
  ("C-c C-l l" . lively)
  ("C-c C-l r" . lively-region)
  ("C-c C-l u" . lively-update)
  ("C-c C-l s" . lively-stop))

(defun eval-last-sexp-other-window (arg)
  "Run `eval-last-sexp' with ARG in the other window."
  (interactive "P")
  (save-window-excursion
    (other-window 1)
    (eval-last-sexp arg)))

(defun expression-to-register (register)
  "Interactively store an Emacs Lisp expression in a REGISTER.
If region is active, store that. Otherwise, store the sexp at
  point."
  (interactive (list (register-read-with-preview "Copy expression to register: ")))
  (set-register register
                (if (region-active-p)
                    (buffer-substring (mark) (point))
                  (destructuring-bind (start . end) (bounds-of-thing-at-point 'sexp)
                    (buffer-substring start end))))
  (setq deactivate-mark t)
  (when (called-interactively-p 'interactive) (indicate-copied-region)))

(defun eval-register (register)
  "Evaluate contents of register REGISTER as an Emacs Lisp expression.
REGISTER is a character and its contents are a string.

If called with a prefix arg, then insert the return value at
point.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (register-read-with-preview "Eval register: ")
                       current-prefix-arg)))
  (let* ((val (get-register register))
         (res (eval (car (read-from-string (format "(progn %s)" val))))))
    (when current-prefix-arg (register-val-insert res))))

(defun replace-last-sexp ()
  "Eval the preceeding sexp and replace it with the result."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(provide 'm-emacs-lisp)

;;; m-emacs-lisp.el ends here
