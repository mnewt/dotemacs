;;; buttonize --- Make a button from a url, address, whatever -*- lexical-binding: t -*-

;; Author: Matthew Newton
;; Maintainer: Matthew Newton
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; Homepage: https://github.com/mnewt/buttonize
;; Keywords: reference, tools, docs


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

;; This package makes buttons out of whatever can be matched using font locking
;; rules. It is primarily intended to be a replacement for `goto-address-mode',
;; but simpler and more flexible.

;; `goto-address-mode' couldn't be extended because it assumes there are exactly
;; two types of interesting things: URLs and email addresses. Turns out there
;; are more.

;;; Code:


;;; Variables

(defface buttonize-1
  '((((class color) (background light)) (:foreground "#1f5bff" :underline t))
    (((class color) (background dark)) (:foreground "#6faaff" :underline t)))
  "First choice face for a button."
  :group 'buttonize)

(defface buttonize-2
  '((((class color) (background light)) (:foreground "#00dd22" :underline t))
    (((class color) (background dark)) (:foreground "#d7ff87" :underline t)))
  "Second choice face for a button."
  :group 'buttonize)

(defface buttonize-3
  `((((class color) (background light)) (:foreground "#555" :underline t))
    (((class color) (background dark)) (:foreground "#eee" :underline t)))
  "Third choice face for a button."
  :group 'buttonize)

(defvar buttonize-mode-keywords nil
  "Font Lock Keywords for `buttonize-mode'.")


;;; Customizeable Features

(defvar buttonize-regexp-url
  (concat (regexp-opt '("file:/" "ftp://" "git://" "http://" "https://" "magnet:"
                        "sftp://" "sip:" "sips:" "smb://" "sms:" "tel:"))
          thing-at-point-url-path-regexp))

(defcustom buttonize-keywords
  `((,buttonize-regexp-url buttonize-1 buttonize-os-open-url-function))
    
  
  "List of elements in the form:

(REGEXP FACE &optional FUNCTION PARAMETERS MODES)

  REGEXP is the REGEXP to match.

  FACE is the FACE to apply.

  FUNCTION is the function to call when the button is activated.

  PARAMETERS is a list of function parameters passed directly the
  function.

  MODES, if non-nil, is a list of modes in which to apply the
  keyword. If it is nil, then the keyword applies to all modes in
  which `buttonize-mode' is activated."
  :group 'buttonize)


 ;;; Functions

(defun buttonize--add-keywords ()
  "Add keywords when activating `buttonize-mode'"
  (cl-loop ((regexp face function parameters modes) buttonize-keywords)))


;;; Commands


;;; Mode

(define-minor-mode buttonize-mode
  "Create buttons around URLs, addresses, and the like."
  nil
  nil
  buttonize-mode-map
  (if buttonize-mode
      (buttonize---add-keywords)
    (buttonize--remove-keywords))
  ;; As of Emacs 24.4, `font-lock-flush' should be used instead of
  ;; `font-lock-fontify-buffer'.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

(define-button-type 'buttonize 'action #'buttonize-button-action 'face 'buttonize-1)

(provide 'eg)

;;; eg.el ends here
