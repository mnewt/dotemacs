;;; m-bind.el --- base key bindings -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton
;; Version: version
;; Package-Requires: (dependencies)
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

;; Define base key bindings

;;; Code:

;; Define "M-m" as a prefix key.
(bind-key "M-m" nil)
(define-prefix-command 'm-map)
(defvar m-map (make-sparse-keymap "M"))

;; Prefix key "M-m i": Insert commands.
(define-prefix-command 'm-insert-map)
(global-set-key (kbd "M-m i") 'm-insert-map)
(defvar m-insert-map (make-sparse-keymap "M-Insert"))

;; Prefix key "M-m w": Window configurations.
(define-prefix-command 'm-window-map)
(global-set-key (kbd "M-m w") 'm-window-map)
(defvar m-insert-map (make-sparse-keymap "M-Window"))

;; Prefix key "M-m f": File operations.
(define-prefix-command 'm-file-map)
(global-set-key (kbd "M-m f") 'm-file-map)
(defvar m-insert-map (make-sparse-keymap "M-File"))

;; Prefix key "M-m h": Help & Hydra.
(define-prefix-command 'm-help-map)
(global-set-key (kbd "M-m h") 'm-help-map)
(defvar m-insert-map (make-sparse-keymap "M-Help"))

;; Key bindings to make moving between Emacs and other appliations a bit less
;; jarring. These are mostly based on macOS defaults but an effor has been made
;; to work on Windows and Linux. That is why there are multiple bindings for
;; many commands. They can be overridden by the OS specific configurations
;; below.

(bind-keys
 ("s-o" . find-file)
 ("s-O" . find-file-other-window)
 ("s-s" . save-buffer)
 ("s-S" . write-file)
 ("s-q" . save-buffers-kill-emacs)
 ("s-z" . undo)
 ("C-z" . undo)
 ("s-a" . mark-whole-buffer)
 ("s-g" . isearch-repeat-forward)
 ("s-G" . isearch-repeat-backward)
 ("C-S-s" . isearch-forward-symbol-at-point)
 ("s-l" . select-current-line)
 ("C-S-L" . select-current-line)
 ("M-o" . other-window)
 ("s-b" . switch-to-buffer)
 ("s-B" . switch-to-buffer-other-window)
 ("s-\`" . other-frame)
 ("C-\`" . other-frame)
 ("s-w" . delete-window)
 ("s-W" . delete-other-windows)
 ("s-C-w" . delete-frame)
 ("s-h" . ns-do-hide-emacs)
 ("s-H" . ns-do-hide-others)
 ("C-c U" . revert-buffer)
 ("s-<return>" . eval-last-sexp)
 ("s-RET" . eval-last-sexp))

(provide 'm-bind)

;;; m-bind.el ends here
