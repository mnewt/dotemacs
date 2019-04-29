;;; highlight-things.el --- Highlight interesting things -*- lexical-binding: t -*-

;; Author: Matthew Newton
;; Maintainer: Matthew Newton
;; Version: 0.0.1
;; Package-Requires: (dash "1.0")
;; Homepage: homepage
;; Keywords: font-lock


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

;; Ideas to filter Eshell output:
;; 1. Eshell feeds each line of program output to
;; `eshell-preoutput-filter-functions'. This is easy but there is no obvious way
;; to handle multi-line semantics.
;; 2. Highlight region from `(eshell-beginning-of-output)' to
;; `(eshell-end-of-output)'. See https://emacs.stackexchange.com/a/5408.
;; 3. Maybe use `org-src-font-lock-fontify-block'.

;; The downsides to #2 and #3 are that the output is not highlighted until after
;; the command finishes.

;; See https://www.emacswiki.org/emacs?SampleMode for mode boilerplate. Note
;; that we are purposefully not using a derived mode because this is not an
;; interactive mode and we don't want to do useless, user-oriented stuff on a
;; temporary buffer. We are going for the most minimal mode possible and it is
;; only intended to be used programmatically so we will eschew the normal
;; boilerplate and mode features like hooks and maps.

;; * TODO: Add a bunch of defcustom's for font-face's of various matches.
;; * TODO: Key in `hlt-keywords-commands' should be a regexp, not a string.
;; * Implement this in the appropriate functions.
;; * TODO: log file 
;; * TODO: Minor mode for netutils
;; * TODO: Keywords: Emacs Lisp functions
;; * TODO: Keywords: Eshell aliases
;; * TODO: Minor mode replace `hl-todo'
;; * TODO: Highlight `shell' output
;; * TODO: Highlight Eshell command line while it's being typed
;; * TODO: Test/support font-lock's matching groups. Do we already support it?

;;; Code:

(defgroup hlt nil
  "Highlight things in buffers."
  :prefix "hlt-"
  :group 'emacs
  :link '(url-link "https://gitlab.com/mnewt/highlight-things"))

(defcustom hlt-face-property-type 'face
  "The property type to use when adding color properties to a string. Normally, you would want this to be 'face, but `font-lock' can overwrite that. If that is happening, try setting this to 'font-lock-face to trick `font-lock' into leaving the coloring alone."
  :type '(symbol)
  :group 'hlt
  :options '(face font-lock-face))

(defcustom hlt-keyword-executable
  `(,(concat "\\<"
             (regexp-opt (-remove (apply-partially #'string-prefix-p ".")
                                  (-distinct (mapcan #'directory-files exec-path)))
                         t)
             "\\>")
    font-lock-keyword-face)
  "Executables found in $PATH."
  :type '(list)
  :group 'hlt)

(defcustom hlt-keyword-eshell-builtin
  (let ((builtins '()))
    (mapatoms (lambda (x)
                (and (fboundp x)
                     (string-prefix-p "eshell/" (symbol-name x))
                     (add-to-list 'builtins (substring (symbol-name x) 7)))))
    builtins)
  "Eshell built in functions, which are just Emacs functions with the `eshell/' prefix"
  :type '(list)
  :group 'hlt)

;; (defcustom hlt-keyword-eshell-alias
;;   (message (eshell/alias)))

(defcustom hlt-keyword-ipv4-address
  (list "\\b\\(?:\\(?:25[0-5]\\|2[0-4][0-9]\\|[01]?[0-9][0-9]?\\)\\.\\)\\{3\\}\\(?:25[0-5]\\|2[0-4][0-9]\\|[01]?[0-9][0-9]?\\)\\b"
        font-lock-string-face)
  "IPv4 address."
  :type '(list)
  :group 'hlt)

(defcustom hlt-keyword-mac-address
  (list "\\b[0-9a-f]\\{2\\}\\(?::[0-9a-f]\\{2\\}\\)\\{5\\}\\b"
        font-lock-string-face)
  "MAC address."
  :type '(list)
  :group 'hlt)

(defcustom hlt-keyword-ipv6-address
  (list "\\b[0-9a-f]\\{4\\}\\(?:::?[0-9a-f]\\{1,4\\}\\)+\\b"
        font-lock-string-face)
  "IPv6 address."
  :type '(list)
  :group 'hlt)

(defcustom hlt-keyword-hostname
  (list "\\b\\(?:\\b[[:alnum:]]+\\.\\)+\\(?:[[:alnum:]]+\\)\\b"
        font-lock-string-face)
  "Hostname."
  :type '(list)
  :group 'hlt)

(defcustom hlt-keyword-human-readable-number
  (list "\\b[0-9.]+[kKmMgGtT]\\b"
        font-lock-builtin-face)
  "Human readable number, like the file size in `ls -h'."
  :type '(list)
  :group 'hlt)

(defcustom hlt-keywords-default '()
  "Keywords which are used when the current command does not have
  a definition in `hlt-keywords-commands'"
  :type '(list)
  :group 'hlt)

(defcustom hlt-keywords-common '()
  "Keywords which are used for all commands."
  :type '(list)
  :group 'hlt)

(defcustom hlt-keywords-commands
  `(("df" ((,(regexp-opt '("Filesystem" "Size" "Used" "Avail" "Use%" "Mounted on") t) font-lock-keyword-face)
           ,hlt-keyword-human-readable-number))
    ("ping" (,hlt-keyword-ipv4-address ,hlt-keyword-ipv6-address ,hlt-keyword-hostname)))
  "Alist. Keyws are strings representing the currently running
  command. Values are regexp/font-face pairs in standard
  font-lock format."
  :type '(list)
  :group 'hlt)

(defvar-local hlt--current-program nil
  "The current program to use for `hlt-mode'.")

(defun hlt--get-keywords (command)
  "Return keywords for specified COMMAND, or default ones if none
  are defined in `hlt-keywords-commands'."
  (or
   (cadr (assoc command hlt-keywords-commands))
   hlt-keywords-default))

(defun hlt-fontify-region (start end &optional keywords)
  "Like `font-lock-fontify-keywords-region' only can be run
without an activated mode."
  ;; Blatantly lie to `font-lock', telling it things have already been set up.
  (let ((font-lock-set-defaults t)
        (font-lock-keywords (or keywords hlt-keywords-default)))
    (font-lock-fontify-keywords-region start end)))

(defun hlt-fontify-string (string &optional keywords)
  "Use `hlt-keywords-default' to fontify the STRING, sort of
like `font-lock-fontify-keywords-region' would, only simpler."
  (dolist (pair (or keywords hlt-keywords-default))
    (let ((re (car pair))
          (face (cdr pair))
          (start 0))
      (while (string-match re string start)
        (add-text-properties (match-beginning 0) (match-end 0)
                             `(,hlt-face-property-type ,face) string)
        (setq start (match-end 0)))))
  string)

(defun hlt-fontify-string-for-command (command string)
  "Like `hlt-fontify-string' but does special stuff for the
specified COMMAND. If COMMAND is nil then fontify using `hlt-keywords-default'"
  (hlt-fontify-string string
                      (append hlt-keywords-common
                              (hlt--get-keywords command))))

(defun hlt-eshell-preoutput-filter (string)
  "Add this to `eshell-preoutput-filter-functions' to highlight Eshell output."
  (hlt-fontify-string-for-command eshell-last-command-name string))

;; (defun hlt-fontify-text (text)
;;   (with-temp-buffer
;;     (erase-buffer)
;;     (insert text)
;;     ;; TODO: Maybe replace `font-lock.el' machinery with our own, for speed.
;;     (setq major-mode 'hlt-mode)
;;     (set (make-local-variable 'font-lock-defaults) '(hlt-keywords-default))
;;     (font-lock-default-function 'hlt-mode)
;;     ;; I can't think of any use cases for syntactic fontification so we skip it.
;;     (font-lock-fontify-keywords-region (point-min) (point-max) nil)
;;     (if hlt-use-font-lock-face
;;         (hlt-replace-face-with-font-lock-face (buffer-string))
;;       (buffer-string))))

;; (defun hlt-replace-face-with-font-lock-face (text)
;;   "Replace property 'face with 'font-lock-face so that font-lock
;; functions don't strip it."
;;   (let ((pos 0))
;;     (while (setq next (next-single-property-change pos 'face text))
;;       (put-text-property pos next 'font-lock-face (get-text-property pos 'face text) text)
;;       (setq pos next))
;;     (add-text-properties 0 (length text) '(fontified t) text)
;;     text))

(defvar hlt-mode-map (make-sparse-keymap)
  "Keymap for `hlt-mode'.")

;;;###autoload
(define-minor-mode hlt-mode
  "Highlight things."
  :lighter ""
  :keymap hlt-mode-map
  :group 'hlt
  (if hlt-mode
      (font-lock-add-keywords nil (hlt--get-keywords hlt--current-command) t)
    (font-lock-remove-keywords nil (hlt--get-keywords hlt--current-command)))
  (when font-lock-mode
    (if (and (fboundp 'font-lock-flush)
             (fboundp 'font-lock-ensure))
        (save-restriction
          (widen)
          (font-lock-flush)
          (font-lock-ensure))
      (with-no-warnings
        (font-lock-fontify-buffer)))))

;;;###autoload
(defun hlt-mode ()
  "Major mode for highlighting things. It is not intended to be
  used interactively, only programmatically."
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults) '(hlt-keywords-default))
  (setq major-mode 'hlt-mode))

(add-hook 'eshell-preoutput-filter-functions 'hlt-eshell-preoutput-filter)
;; (remove-hook 'eshell-preoutput-filter-functions 'hlt-eshell-preoutput-filter)

(provide 'highlight-things)

;;; highlight-things.el ends here
