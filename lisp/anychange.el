;;; anychange.el --- Go to changes in any open buffer -*- lexical-binding: t -*-

;; Author: Matthew Newton
;; Maintainer: Matthew Newton
;; Version: 0.1
;; Package-Requires: ()
;; Homepage: https://gitlab.com/mnewt/anychange
;; Keywords: emacs


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

;; commentary

;;; Code:

(defcustom anychange-minimum-distance 100
  "Minium number of characters between the last change and the
current one before we record a new change."
  :type 'number
  :group 'anychange)
.
(defvar anychange-positions-list '()
  "List of changes.")

(defvar anychange-position 0
  "Current position in the list.")

(defun anychange--minimum-distance-p (a b)
  "Return `t' if A is at least the minimum distance from B."
  (>= (abs (- a b)) anychange-minimum-distance))

(defun anychange--get-nth-position (n)
  "Return the N-th position from `anychange-positions-list'."
  (cdr (nth n anychange-positions-list)))

(defun anychange--get-nth-situation (n)
  "Return relevant attributes from the N-th position in `anychange-positions-list'.

They are returned as a list.
"
  (if-let* ((change (nth n anychange-positions-list))
            (name (car change))
            (position (cdr change))
            (buffer-or-file (or (get-buffer name) (when (file-exists-p name)
                                                    (find-file-noselect name))))
            (window (get-buffer-window buffer-or-file)))
      (list position buffer-or-file window)
    (list nil nil nil)))

(defun anychange-go (&optional direction)
  "According to DIRECTION, go `forward' or `backward' on
`anychange-positions-list' and find the change that is is at
least `anychange-minimum-distance' characters away."
  (interactive "P")
  (setq direction (or direction 1))
  (when current-prefix-arg (setq direction (- direction)))
  (setq n (+ anychange-position direction))
  (while (and (< n (length anychange-positions-list))
              (not (anychange--minimum-distance-p (point) (anychange--get-nth-position n))))
    (setq n (+ n (if (natnump direction) 1 -1))))
  (destructuring-bind (position buffer-or-file window) (anychange--get-nth-situation n)
    (if buffer-or-file
        (progn
          (cond
           (window (select-window window))
           ((buffer-live-p buffer-or-file-name) (switch-to-buffer buffer-or-file))
           ((file-exists-p buffer-or-file) (find-file buffer-or-file)))
          (goto-char position)
          (message "anychange number %d: buffer: %s, position %d"
                   anychange-position buffer-or-file  position)
          (incf anychange-position))
      (message "anychange: Couldn't find the buffer or file: %d"
               buffer-or-file))))

(defalias 'anychange-go-forward #'anychange-go
  "Go forward through the positions list.")

(defun anychange-go-backward ()
  "Go backward through the positions list."
  (interactive "P")
  (if current-prefix-arg (anychange-go 1) (anychange-go -1)))

(defun anychange-buffer-change-hook (beg end len)
  (setq anychange-position 0)
  (let ((buf (or (buffer-file-name) (buffer-name)))
        (previous-position (when anychange-positions-list
                             (cdar anychange-positions-list))))
    (message "buf: %s" buf)
    (if previous-position
        (message "previous-position: %d"previous-position)
      (message "No previous position"))
    (when (and buf (not (string-match-p "\\`\\(?: ?\\*\\)" buf))
               (or (not previous-position)
                   (>= (abs (- end previous-position)) anychange-minimum-distance)))
      (setq anychange-positions-list
            (cons
             (cons buf end)
             (subseq anychange-positions-list anychange-position)))
      (message "anychange: %s : %d" buf end))))

(defvar anychange-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c '") #'anychange-go-forward)
    (define-key map (kbd "C-c ;") #'anychange-go-backward)
    map)
  "Keymap used when `anychange-mode' is active.")

;;;###autoload
(define-minor-mode anychange-mode
  "Toggle `anychange-mode': Track changes across all buffers and
then navigate back to them."
  :global t
  :lighter " Ⓐ"
  :keymap anychange-mode-map
  :group 'anychange
  (if anychange-mode
      (add-hook 'after-change-functions #'anychange-buffer-change-hook)
    (remove-hook 'after-change-functions #'anychange-buffer-change-hook)))

(provide 'anychange)

(setq anychange-position 0
      anychange-positions-list nil)

;;; anychange.el ends here
.
