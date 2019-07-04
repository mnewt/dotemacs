;;; dired-rainbow-x.el --- Dired Rainbow Extensions -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton
;; Version: 0.1
;; Package-Requires: ((dired-rainbow ""))
;; Homepage: /
;; Keywords: dired


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

;; My own personal extensions to `dired-rainbow'. It's possible I'll try and add
;; some of them upstream at some point.

;;; Code:

(require 'dired-hacks-utils)

(defface dired-rainbow-permissions '((t (:inherit default)))
  "Face for Dired permissions."
  :group 'dired-rainbow)

(defface dired-rainbow-inodes '((t (:inherit shadow)))
  "Face for Dired links."
  :group 'dired-rainbow)

(defface dired-rainbow-user '((t (:inherit default)))
  "Face for Dired user."
  :group 'dired-rainbow)

(defface dired-rainbow-group '((t (:inherit font-lock-comment-face)))
  "Face for Dired group."
  :group 'dired-rainbow)

(defface dired-rainbow-size '((t (:inherit default)))
  "Face for Dired file size."
  :group 'dired-rainbow)

(defface dired-rainbow-datetime
  '((((background dark)) (:inherit default :foreground "#999"))
    (t (:inherit default :foreground "#777")))
  "Face for Dired timestamp."
  :group 'dired-rainbow)

(defface dired-rainbow-file-extension
  '((((background dark)) (:inherit default :foreground "#444"))
    (t (:inherit default :foreground "#BBB")))
  "Face for Dired file extensions."
  :group 'dired-rainbow)

(defface dired-rainbow-file-decoration '((t (:inherit default)))
  "Face for file decoration."
  :group 'dired-rainbow)

(defface dired-rainbow-dash
  '((((background dark)) (:inherit default :foreground "#777"))
    (t (:inherit default :foreground "#999")))
  "Face for file decoration."
  :group 'dired-rainbow)

(defface dired-rainbow-filetype-directory '((t (:inherit font-lock-function-name-face)))
  "Face for file decoration."
  :group 'dired-rainbow)

(defface dired-rainbow-filetype-link '((t (:inherit font-lock-string-face)))
  "Face for file decoration."
  :group 'dired-rainbow)

(defvar dired-rainbow-permissions-regexp "[-dl][-rwxlsStT]\\{9\\}[.+-@]?"
  "A regexp matching the permissions in the dired listing.")

(defvar dired-rainbow-inodes-regexp "[0-9]+"
  "A regexp matching the number of links in the dired listing.")

(defvar dired-rainbow-user-or-group-regexp "[a-z_][a-z0-9_-]*"
  "A regexp matching the user and group in the dired listing.")

(defvar dired-rainbow-size-regexp "[0-9.]+[kKmMgGtTpPi]\\{0,3\\}"
  "A regexp matching the file size in the dired listing.")

(defvar dired-rainbow-file-extension-regexp "\\.[^./]*?$"
  "A regexp matching file extensions.")

(defvar dired-rainbow-file-decoration-regexp
  "\\(?:[*/]\\| -> .*?\\(\\.*?\\)?\\)?"
  "A regexp matching the file decoration in the dired listing.

This is the `/', `*', or ` -> file_name' after the file name when
the `ls -F' option is used.

It should be wrapped in an optional capture group.")

(defvar dired-rainbow-details-regexp
  (let ((sep "\\) +\\("))
    (concat "^ +\\("
            dired-rainbow-permissions-regexp sep
            dired-rainbow-inodes-regexp sep
            dired-rainbow-user-or-group-regexp sep
            dired-rainbow-user-or-group-regexp sep
            dired-rainbow-size-regexp sep
            dired-hacks-datetime-regexp
            "\\)")))

(defvar dired-rainbow-ending-regexp
  (concat "\\("
          dired-rainbow-file-extension-regexp "\\)\\("
          dired-rainbow-file-decoration-regexp "\\)$"))

(defvar dired-rainbow-listing-keywords
  `((,(concat "\\(total used in directory\\|available\\) +\\("
              dired-rainbow-size-regexp "\\)")
     (1 'font-lock-comment-face)
     (2 'default))
    ("^ +\\(-\\)" 1 'dired-rainbow-dash)
    ("^ +\\(d\\)" 1 'dired-rainbow-filetype-directory)
    ("^ +\\(l\\)" 1 'dired-rainbow-filetype-link)
    ("^ +.\\(-\\)" 1 'dired-rainbow-dash)
    ("^ +..\\(-\\)" 1 'dired-rainbow-dash)
    ("^ +...\\(-\\)" 1 'dired-rainbow-dash)
    ("^ +....\\(-\\)" 1 'dired-rainbow-dash)
    ("^ +.....\\(-\\)" 1 'dired-rainbow-dash)
    ("^ +......\\(-\\)" 1 'dired-rainbow-dash)
    ("^ +.......\\(-\\)" 1 'dired-rainbow-dash)
    ("^ +........\\(-\\)" 1 'dired-rainbow-dash)
    ("^ +.........\\(-\\)" 1 'dired-rainbow-dash)
    (,dired-rainbow-details-regexp
     (1 'dired-rainbow-permissions)
     (2 'dired-rainbow-inodes)
     (3 'dired-rainbow-user)
     (4 'dired-rainbow-group)
     (5 'dired-rainbow-size)
     (6 'dired-rainbow-datetime))
    (,dired-rainbow-file-extension-regexp 0 'dired-rainbow-file-extension t)))

(define-minor-mode dired-rainbow-listing-mode
  "Toggle highlighting of file listing details in Dired."
  :group 'dired-rainbow
  :lighter ""
  (progn
    (if dired-rainbow-listing-mode
        (progn
          (setq dired-rainbow-listing-mode t)
          (font-lock-add-keywords 'dired-mode dired-rainbow-listing-keywords 'end))
      (font-lock-remove-keywords 'dired-mode dired-rainbow-listing-keywords))
    (mapc (lambda (b) (with-current-buffer b
                        (when (equal major-mode 'dired-mode)
                          (font-lock-refresh-defaults))))
          (buffer-list))))

(provide 'dired-rainbow-x)

;;; dired-rainbow-x.el ends here
