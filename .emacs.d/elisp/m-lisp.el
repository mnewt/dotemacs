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

(use-package parinfer
  :custom
  (parinfer-extensions
   '(defaults       ; should be included.
      pretty-parens ; different paren styles for different modes.
      smart-tab     ; C-b & C-f jump positions and smart shift with tab & S-tab.
      smart-yank))  ; Yank behavior depends on mode.
  :config
  (parinfer-strategy-add 'default 'newline-and-indent)
  :hook
  ((clojure-mode common-lisp-mode emacs-lisp-mode hy-mode lisp-interaction-mode
                 lisp-mode scheme-mode) . parinfer-mode)
  :bind
  (:map parinfer-mode-map
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)
        ("C-i" . parinfer--reindent-sexp)
        ("C-M-i" . parinfer-auto-fix)
        ("C-," . parinfer-toggle-mode)
        ;; Don't interfere with smartparens quote handling
        ("\"" . nil)
        ;; sp-newline seems to offer a better experience for lisps
        ("RET" . sp-newline)
        ("<return>" . sp-newline)
        :map parinfer-region-mode-map
        ("C-i" . indent-for-tab-command)
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)))

(use-package lisp-extra-font-lock
  :config
  (lisp-extra-font-lock-global-mode 1))

(defvar flash-region-ovl nil
  "The overlay used for flash.")
(make-variable-buffer-local 'flash-region-ovl)

(defvar flash-region-face 'highlight
  "The face used for flash.")

(defvar flash-region-timeout 0.5
  "The number of seconds the flash shall last.")

(defun flash-region--remove-ovl (buf)
  "Remove the flash overlay if it exists in BUF."
  (with-current-buffer buf
    (when (overlayp flash-region-ovl)
      (delete-overlay flash-region-ovl))
    (setq flash-region-ovl nil)))

(defun flash-region (beg end &optional face timeout)
  "Show an overlay from BEG to END using FACE to set display
properties. The overlay automatically vanishes after TIMEOUT
seconds."
  (interactive "r")
  (let ((face (or face flash-region-face))
        (timeout (or (and (numberp timeout) (< 0 timeout) timeout)
                     flash-region-timeout)))
    (flash-region--remove-ovl (current-buffer))
    (setq flash-region-ovl (make-overlay beg end))
    (overlay-put flash-region-ovl 'face face)
    (when (< 0 timeout)
      (run-with-idle-timer timeout
                           nil 'flash-region--remove-ovl (current-buffer)))))

(defun flash-last-sexp (_)
  (flash-region (point) (save-excursion (backward-sexp) (point))))

(defun flash-last-sexp-other-window (_)
  "Run `eval-last-sexp' with ARG in the other window."
  (save-window-excursion
    (other-window 1)
    (flash-last-sexp nil)))

(defun flash-defun (_)
  (flash-region (save-excursion (beginning-of-defun) (point))
                (save-excursion (end-of-defun) (point))))

(advice-add 'eval-region :before #'flash-region)
(advice-add 'eval-last-sexp :before #'flash-last-sexp)
(advice-add 'eval-print-last-sexp :before #'flash-last-sexp)
(advice-add 'eval-defun :before #'flash-defun)
(with-eval-after-load 'crux
  (advice-add 'crux-eval-and-replace :before #'flash-last-sexp))
(with-eval-after-load 'cider
  (advice-add 'cider-eval-region :before #'flash-region)
  (advice-add 'cider-eval-last-sexp :before #'flash-last-sexp)
  (advice-add 'cider-eval-last-sexp-and-append :before #'flash-last-sexp)
  (advice-add 'cider-eval-last-sexp-in-context :before #'flash-last-sexp)
  (advice-add 'cider-pprint-eval-last-sexp :before #'flash-defun)
  (advice-add 'cider-pprint-eval-last-sexp-to-comment :before #'flash-defun)
  (advice-add 'cider-pprint-eval-last-sexp-to-repl :before #'flash-defun)
  (advice-add 'cider-eval-defun-at-point :before #'flash-defun)
  (advice-add 'cider-eval-defun-at-point-in-context #'flash-defun)
  (advice-add 'cider-eval-defun-to-comment :before #'flash-defun)
  (advice-add 'cider-pprint-eval-defun-to-comment :before #'flash-defun)
  (advice-add 'cider-pprint-eval-defun-to-repl :before #'flash-defun))

;; (use-package lively
;;   :commands
;;   (lively-shell-command)
;;   :bind
;;   ("C-c C-l l" . lively)
;;   ("C-c C-l r" . lively-region)
;;   ("C-c C-l u" . lively-update)
;;   ("C-c C-l s" . lively-stop))

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

;; (use-package sly
;;   ;; There are some problems building sly with straight.el in Windows
;;   :unless (eq system-type 'windows-nt)
;;   :custom
;;   (inferior-lisp-program (executable-find "sbcl"))
;;   :bind
;;   (:map sly-prefix-map
;;         ("M-h" . sly-documentation-lookup)))

;; (use-package sly-company
;;   :unless (eq system-type 'windows-nt)
;;   :hook
;;   (sly-mode . sly-company-mode)
;;   :config
;;   (add-to-list 'company-backends 'sly-company))

;; Configured to use CHICKEN Scheme
;; (use-package geiser
;;   :custom
;;   (geiser-default-implementation 'chicken)
;;   (geiser-mode-eval-last-sexp-to-buffer t)
;;   (scheme-program-name "csi -:c")
;;   :config
;;   (setq-default geiser-scheme-implementation 'chicken)

;;   ;; Indenting module body code at column 0
;;   (defun scheme-module-indent (state indent-point normal-indent) 0)
;;   (put 'module 'scheme-indent-function 'scheme-module-indent)
;;   (put 'and-let* 'scheme-indent-function 1)
;;   (put 'parameterize 'scheme-indent-function 1)
;;   (put 'handle-exceptions 'scheme-indent-function 1)
;;   (put 'when 'scheme-indent-function 1)
;;   (put 'unless 'scheme-indenfunction 1)
;;   (put 'match 'scheme-indent-function 1)
;;   :commands
;;   (geiser run-geiser run-chicken))

(require 'crux)

(bind-keys
 ("C-c C-j" . crux-eval-and-replace)
 :map lisp-mode-shared-map
 ("s-<return>" . eval-last-sexp)
 ("C-s-<return>" . eval-last-sexp-other-window)
 ("C-c C-k" . eval-buffer)
 ("C-x C-r" . eval-region)
 ("C-x r E" . expression-to-register)
 ("C-x r e" . eval-register))

(provide 'm-lisp)

;;; m-lisp.el ends here
