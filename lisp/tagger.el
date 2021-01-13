;;; tagger.el --- Organize Org Files by Tag -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (spinner "1.0.0"))
;; Homepage: https://github.com/mnewt/tagger
;; Keywords: docs files hypermedia outlines


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

;; Browse and find Org files by tag, with a loose definition of tag.

;;; Code:

(require 'org)
(require 'spinner)

(defgroup tagger nil
  "Search in Org files by tag."
  :group 'org
  :prefix "tagger-")

(defcustom tagger-list-functions
  '(tagger-list-org-tags
    tagger-list-org-headlines
    tagger-list-org-file-names)
  "List of functions which produce a list of tags."
  :type '(repeat symbol))

(defcustom tagger-tag-line-regexp
  "^(?:\*+|#\+FILETAGS:|#\+filetags:)[\t\s]+.*:[a-z]+:"
  "Regexp matching a line with tags on it.

Note that this regexp is passed to ripgrep."
  :type 'string)

(defvar-local tagger--entries (make-hash-table :test #'equal)
  "Internal representation of tag entries.")

(defvar-local tagger--tag-buffer nil
  "The main tag listing buffer.")

(defvar-local tagger--tag nil
  "The current tag for the `tagger-line-list-mode' buffer.")

(defvar-local tagger--lines nil
  "The current line entries for the `tagger-line-list-mode' buffer.")

;; TODO: Eliminate this.
(defvar tagger-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'tagger-follow-link)
    (define-key map (kbd "<return>") #'tagger-follow-link)
    (define-key map [mouse-1] #'tagger-follow-link)
    map)
  "Keys in effect when point is over a link in Tagger.")

(defun tagger--help-echo (_window _object pos)
  "Display `help-echo' for WINDOW OBJECT and POS."
  (when-let ((button (button-at pos)))
    (message "tag: %s" (button-get button :link))))

(defun tagger-tag-button-action (button)
  "The default action for BUTTON type `tagger-tag'."
  (tagger-list-lines (button-get button :tag)))

(define-button-type 'tagger-tag
  :help-echo #'tagger--help-echo
  :action #'tagger-tag-button-action)

(defun tagger--add-tag-line (tag file line)
  "Add TAG, FILE, and LINE to the tagger entries.

Also update `tabulated-list-entries' and refresh the display.

This function is intended to be invoked in a Tagger buffer."
  (puthash tag
           (cons (list file line) (gethash tag tagger--entries))
           tagger--entries)
  (setf (alist-get tag tabulated-list-entries nil nil 'equal)
        (list (vector `(,tag :type tagger-tag :tag ,tag)
                      (number-to-string (length (gethash tag tagger--entries))))))
  (tabulated-list-print 'remember-pos))

(defun tagger-refresh-tags ()
  "Recompute the list of tags for the Tagger buffer."
  (spinner-start)
  (setq tagger--entries (make-hash-table :test #'equal)
        tabulated-list-entries nil)
  (tagger--list-ripgrep))

(defun tagger-tags ()
  "Return a list of tags."
  (with-current-buffer "*Tagger*"
    (let (keys)
      (maphash (lambda (key _value) (push key keys)) tagger--entries)
      keys)))

(defun tagger-find-file (filename line)
  "Find FILENAME and open it at LINE."
  (message "tagger-find-file")
  (find-file filename)
  (goto-char (point-min))
  (forward-line line))

(defun tagger-link-button-action (button)
  "The action for BUTTON type `tagger-link'."
  (tagger-find-file (button-get button :file) (button-get button :line)))

(define-button-type 'tagger-link
  :help-echo #'tagger-help-echo
  :action #'tagger-link-button-action)

(defun tagger-refresh-lines ()
  "Recompute the list of lines for the Tagger Lines buffer."
  (setq tagger--lines
        (gethash tagger--tag (buffer-local-value 'tagger--entries (get-buffer "*Tagger*"))))
  (setq tabulated-list-entries
        (mapcar (lambda (entry)
                  (cl-destructuring-bind (file line) entry
                    (list entry
                          (vector `(,(file-name-nondirectory file)
                                    :type tagger-link
                                    :file ,file
                                    :line ,line)
                                  "" (number-to-string line)))))
                tagger--lines))
  (tabulated-list-print 'remember-pos))

(defun tagger--grep-file-and-line-number ()
  "Get the file name and line number at point.

The expected format is like that from 'grep -n'.

After parsing, leave point at the start of the line contents."
  (beginning-of-line)
  (list (buffer-substring-no-properties (point) (1- (search-forward ":")))
        (string-to-number
         (buffer-substring-no-properties (point) (1- (search-forward ":"))))))

(defun tagger--grep-org-line ()
  "Process one line of grep results.

Results are expected to be in the current buffer, with each line
in the format:

<path-to-file>:<line-number>:<line-contents>

Return a list of the form:
  (\"tag\" \"/path/to/file\" <line-number>)

Return nil if there is no complete line at point, or there are no
tags on the line.

Note that a line is considered complete if it has a newline after
it."
  ;; Only process a line if it is complete.
  (when (char-after (line-end-position))
    (cl-destructuring-bind (file-name line-number) (tagger--grep-file-and-line-number)
      (let (tags)
        (while (search-forward-regexp ":\\([[:word:]]+\\):" (line-end-position) 'noerror)
          (push (buffer-substring-no-properties (match-beginning 1) (match-end 1)) tags)
          (backward-char))
        (search-forward "\n")
        (mapcar (lambda (tag) (list tag file-name line-number)) tags)))))

(defun tagger--async-filter-grep-org (proc string)
  "Process output STRING from PROC `tagger--list-ripgrep'."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (save-excursion
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
      ;; Add results to the list, starting where we left off.
      (let (entries)
        (while (setq entries (tagger--grep-org-line))
          (with-current-buffer (process-get proc :tagger-buffer)
            (dolist (entry entries)
              (tagger--add-tag-line (car entry) (cadr entry) (caddr entry))))))
      (goto-char (process-mark proc)))))

(defun tagger--async-sentinel (proc event)
  "Process output for PROC after EVENT."
  (if (equal event "finished\n")
      (with-current-buffer (process-get proc :tagger-buffer)
        (spinner-stop))
    (message "Tagger received signal: %s" (string-trim event))))

(defun tagger--list-ripgrep ()
  "Find tags and update the Tagger buffer."
  (let ((buffer (get-buffer-create " *tagger-list-ripgrep*"))
        proc)
    (with-current-buffer buffer
      (erase-buffer))
    (setq proc
          (make-process
           :name "tagger-list-ripgrep"
           :buffer buffer
           :command `("rg"
                      "--color=never"
                      "--no-heading"
                      "--with-filename"
                      "(?:\\*+|#\\+FILETAGS:|#\\+filetags:)[\\t\\s]+.*:[a-z]+:"
                      "-g"
                      "*.org"
                      ,(expand-file-name org-directory))
           :noquery t
           :filter #'tagger--async-filter-grep-org
           :sentinel #'tagger--async-sentinel))
    (process-put proc :tagger-buffer (get-buffer "*Tagger*"))))

(defun tagger-string-number-greaterp (a b)
  "Return non-nil if A is greater than B."
  (> (string-to-number (aref (cadr a) 1))
     (string-to-number (aref (cadr b) 1))))

(define-derived-mode tagger-tag-list-mode tabulated-list-mode "Tagger Tags"
  "Major mode for listing tags."
  (setq tabulated-list-format [("tag" 12 t)
                               ("number" 7 tagger-string-number-greaterp (:right-align t))]
        tabulated-list-padding 2
        tabulated-list-sort-key '("tag" . nil))
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook #'tagger-refresh-tags nil t))

;;;###autoload
(defun tagger ()
  "Display a list of tags."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Tagger*"))
  (tagger-tag-list-mode)
  (tagger-refresh-tags))

(define-derived-mode tagger-line-list-mode tabulated-list-mode "Tagger Lines"
  "Major mode for listing tagged lines."
  (setq tabulated-list-format [("file" 20 t)
                               ("modified" 10 t)
                               ("line" 50 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key '("file" . nil))
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook #'tagger-refresh-lines nil t))

;;;###autoload
(defun tagger-list-lines (tag)
  "Display a list of lines for a TAG."
  (interactive (list (completing-read "List lines for tag: " (tagger-tags))))
  (switch-to-buffer (get-buffer-create (format "*Tagger Tag: %s*" tag)))
  (tagger-line-list-mode)
  (setq tagger--tag tag)
  (tagger-refresh-lines))

(provide 'tagger)

;;; tagger.el ends here
