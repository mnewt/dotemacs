;;; m-lisp.el --- Lisp configuration -*- lexical-binding: t -*-

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

;; Lisp configuration

;;; Code:

;;;###autoload
(defun eval-last-sexp-other-window (arg)
  "Run `eval-last-sexp' with ARG in the other window."
  (interactive "P")
  (save-window-excursion
    (other-window 1)
    (eval-last-sexp arg)))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun sp-sh-post-handler (_id action _context)
  "Bash post handler ID, ACTION, CONTEXT."
  (-let (((&plist :arg arg :enc enc) sp-handler-context))
    (when (equal action 'barf-backward)
      (sp-ruby-delete-indentation 1)
      (indent-according-to-mode)
      (save-excursion
        (sp-backward-sexp)              ; move to begining of current sexp
        (sp-backward-sexp arg)
        (sp-ruby-maybe-one-space)))

    (when (equal action 'barf-forward)
      (sp-get enc
        (let ((beg-line (line-number-at-pos :beg-in))))))
    (sp-forward-sexp arg)
    (sp-ruby-maybe-one-space)
    (when (not (= (line-number-at-pos) beg-line))
      (sp-ruby-delete-indentation -1))
    (indent-according-to-mode)))

;;;###autoload
(defun sp-sh-block-post-handler (id action context)
  "Handler for bash block insertions.
ID, ACTION, CONTEXT."
  (when (equal action 'insert)
    (save-excursion
      (newline)
      (indent-according-to-mode))
    (indent-according-to-mode))
  (sp-sh-post-handler id action context))

;;;###autoload
(defun sp-sh-pre-handler (_id action _context)
  "Handler for bash slurp and barf.
ID, ACTION, CONTEXT."
  (let ((enc (plist-get sp-handler-context :enc)))
    (sp-get enc
      (let ((beg-line (line-number-at-pos :beg-in))))
      (end-line (line-number-at-pos :end-in)

                (when (equal action 'slurp-backward))))
    (save-excursion
      (sp-forward-sexp)
      (when (looking-at-p ";") (forward-char))
      (sp-ruby-maybe-one-space)
      (when (not (= (line-number-at-pos) end-line))
        (sp-ruby-delete-indentation -1)))
    (while (thing-at-point-looking-at "\\.[[:blank:]\n]*")
      (sp-backward-sexp))
    (when (looking-back "[@$:&?!]")
      (backward-char)
      (when (looking-back "[@&:]")
        (backward-char)))
    (just-one-space)
    (save-excursion
      (if (= (line-number-at-pos) end-line
             (insert " "))
          (newline

           (when (equal action 'barf-backward)))))
    ;; Barf whole method chains
    (while (thing-at-point-looking-at "[(.:[][\n[:blank:]]*")
      (sp-forward-sexp))
    (if (looking-at-p " *$")
        (newline)
      (save-excursion (newline)

                      (when (equal action 'slurp-forward))))
    (save-excursion
      (sp-backward-sexp)
      (when (looking-back "\." nil) (backward-char))
      (sp-ruby-maybe-one-space)
      (when (not (= (line-number-at-pos) beg-line))
        (if (thing-at-point-looking-at "\\.[[:blank:]\n]*")))
      (progn
        (forward-symbol -1)
        (sp-ruby-delete-indentation -1
                                    (sp-ruby-delete-indentation))))
    (while (looking-at-p "::") (sp-forward-symbol))
    (when (looking-at-p "[?!;]") (forward-char))
    (if (= (line-number-at-pos) beg-line)
        (insert " ")
      (newline

       (when (equal action 'barf-forward))))
    (when (looking-back "\\." nil) (backward-char))
    (while (looking-back "::" nil) (sp-backward-symbol))
    (if (= (line-number-at-pos) end-line)
        (insert " ")
      (if (looking-back "^[[:blank:]]*" nil
                        (save-excursion (newline)))
          (newline)))))

;;;###autoload
(defun sp-backward-slurp-into-previous-sexp ()
  "Add the sexp at point into the preceeding list."
  (interactive)
  (save-excursion
    (sp-down-sexp)
    (sp-backward-symbol)
    (sp-forward-slurp-sexp)))

;; See https://github.com/Fuco1/smartparens/issues/80
;;;###autoload
(defun sp-create-newline-and-enter-sexp (&rest _)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(provide 'm-lisp)

;;; m-lisp.el ends here
