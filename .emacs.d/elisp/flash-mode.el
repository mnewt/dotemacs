;;; flash-mode.el --- Flash things when they are evaluated -*- lexical-binding: t -*-

;; Author: Matthew Newton
;; Maintainer: Matthew Newton
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/mnewt/flash-mode
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

;; Flash mode flashes Emacs S-expressions and Commands when they are evaluated.
;;
;; It supports the following modes and packages by default:
;;
;; * emacs-lisp-mode
;; * lisp-interaction-mode
;; * CIDER
;; * inf-clojure
;; * SLIME
;; * Sly
;;
;; It can be easily extended to other packages by customizing
;; `flash-commands'.
;;
;;; Code:

(defcustom flash-face 'highlight
  "The face used for flash."
  :group 'flash-mode
  :type 'symbol)

(defcustom flash-timeout 0.5
  "The number of seconds the flash shall last."
  :group 'flash-mode
  :type 'number)

(defcustom flash-commands
  '((elisp-mode
     (eval-buffer . flash-buffer)
     (eval-region . flash-region)
     (eval-last-sexp . flash-last-sexp)
     (eval-print-last-sexp . flash-last-sexp)
     (eval-defun . flash-defun)
     (crux-eval-and-replace . flash-last-sexp))

    (cider
     (cider-eval-region . flash-region)
     (cider-eval-last-sexp . flash-last-sexp)
     (cider-eval-last-sexp-and-append . flash-last-sexp)
     (cider-eval-last-sexp-in-context . flash-last-sexp)
     (cider-pprint-eval-last-sexp . flash-defun)
     (cider-pprint-eval-last-sexp-to-comment . flash-defun)
     (cider-pprint-eval-last-sexp-to-repl . flash-defun)
     (cider-eval-defun-at-point . flash-defun)
     (cider-eval-defun-at-point-in-context-. fun)
     (cider-eval-defun-to-comment . flash-defun)
     (cider-pprint-eval-defun-to-comment . flash-defun)
     (cider-pprint-eval-defun-to-repl . flash-defun))

    (inf-clojure
     (inf-clojure-eval-buffer . flash-buffer)
     (inf-clojure-eval-last-sexp . flash-last-sexp))

    (slime
     (slime-eval-last-expression . flash-last-sexp)
     (slime-pprint-eval-last-expression . flash-last-sexp)
     (slime-eval-defun . flash-defun))

    (sly
     (sly-eval-last-expression . flash-last-sexp)
     (sly-pprint-eval-last-expression . flash-last-sexp)
     (sly-eval-defun . flash-defun)))

  "An alist where car is the a file (feature) and cdr is a list
  of pairs. Flash mode loads after the file or feature is loaded.
  The pairs are a command that we want to flash followed by a
  flash function."
  :group 'flash-mode
  :type 'list)

(defvar flash-region-overlay nil
  "The overlay used for flash.")
(make-variable-buffer-local 'flash-region-overlay)

(defun flash-region--remove-overlay (buf)
  "Remove the flash overlay if it exists in BUF."
  (with-current-buffer buf
    (when (overlayp flash-region-overlay)
      (delete-overlay flash-region-overlay))
    (setq flash-region-overlay nil)))

(defun flash-region (beg end &optional face timeout)
  "Show an overlay from BEG to END using FACE to set display
properties. The overlay automatically vanishes after TIMEOUT
seconds."
  (interactive "r")
  (let ((face (or face flash-face))
        (timeout (or (and (numberp timeout) (< 0 timeout) timeout)
                     flash-timeout)))
    (flash-region--remove-overlay (current-buffer))
    (setq flash-region-overlay (make-overlay beg end))
    (overlay-put flash-region-overlay 'face face)
    (when (< 0 timeout)
      (run-with-idle-timer timeout
                           nil 'flash-region--remove-overlay (current-buffer)))))

(defun flash-last-sexp (&rest _)
  "Flash the S-expression before point."
  (flash-region (point) (save-excursion (backward-sexp) (point))))

(defun flash-last-sexp-other-window (&rest _)
  "Flash the S-expression before point in the other window."
  (save-window-excursion
    (other-window 1)
    (flash-last-sexp nil)))

(defun flash-defun (&rest _)
  "Flash the defun surrounding point."
  (flash-region (save-excursion (beginning-of-defun) (point))
                (save-excursion (end-of-defun) (point))))

(defun flash-line (&rest _)
  "Flash the line."
  (flash-region (point-at-bol) (point-at-eol)))

(defun flash-buffer (&rest _)
  "Flash the whole buffer."
  (flash-region (point-min) (point-max)))

(defun* flash-mode--add-advice-to-command ((command . function))
  (advice-add command :before function))

(defun* flash-mode--add-advice-to-file ((file . commands))
  (with-eval-after-load file
    (mapc #'flash-mode--add-advice-to-command commands)))

(defun flash-mode--add-advice ()
  (mapc #'flash-mode--add-advice-to-file flash-commands))

(defun* flash-mode--remove-advice-from-command ((command . function))
  (advice-remove command function))

(defun* flash-mode--remove-advice-from-file ((file . commands))
  (with-eval-after-load file
    (mapc #'flash-mode--remove-advice-from-command commands)))

(defun flash-mode--remove-advice ()
  "Remove advice from the commands supported by `flash-mode'."
  (mapc #'flash-mode--remove-advice-from-file flash-commands))

(define-minor-mode flash-mode
  "Toggle `flash-mode' on or off.

When the mode is on and a sexp is evaluated, `flash-mode' causes
the sexp to flash briefly."
  :init-value t
  :global t
  (if flash-mode
      (flash-mode--add-advice)
    (flash-mode--remove-advice)))

(provide 'flash-mode)

;;; flash-mode.el ends here
