;;; m-org.el --- My Org config -*- lexical-binding: t -*-

;; Author: Matthew Newton
;; Maintainer: Matthew Newton
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

;; commentary

;;; Code:

(require 'org)

;;;###autoload
(defun search-org-files ()
  "Search ~/org using `counsel-rg'."
  (interactive)
  (counsel-rg nil org-directory))

;;;###autoload
(defun org-todo-todo ()
  "Create or update Org todo entry to TODO status."
  (interactive)
  (org-todo "TODO"))

(defun org-todo-to-int (todo)
  "Get the number of the TODO based on its status."
  (first (-non-nil
          (mapcar (lambda (keywords)
                    (let ((todo-seq
                           (-map (lambda (x) (first (split-string  x "(")))
                                 (rest keywords))))
                      (cl-position-if (lambda (x) (string= x todo)) todo-seq)))
                  org-todo-keywords))))

(defun org-sort-entries--todo-status-key ()
  "Sort Org TODO entries by their status."
  (let* ((todo-max (apply #'max (mapcar #'length org-todo-keywords)))
         (todo (org-entry-get (point) "TODO"))
         (todo-int (if todo (org-todo-to-int todo) todo-max))
         (priority (org-entry-get (point) "PRIORITY"))
         (priority-int (if priority (string-to-char priority) org-default-priority)))
    (format "%03d %03d" todo-int priority-int)))

;;;###autoload
(defun org-sort-entries-by-todo-status ()
  "Sort Org TODO entries by their status."
  (interactive)
  (org-sort-entries nil ?f #'org-sort-entries--todo-status-key))

(setq
 org-directory "~/org"
 ;; Clean view
 org-startup-indented t
 ;; Smart C-a/e
 org-special-ctrl-a/e t
 ;; Smart C-k
 org-special-ctrl-k t
 ;; Insert a row in tables
 org-special-ctrl-o t
 ;; Tab in source blocks should act like in major mode
 org-src-tab-acts-natively t
 ;; Code highlighting in code blocks
 org-src-fontify-natively t
 ;; Customize todo keywords
 ;; (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WIP(w)" "|" "DONE(d!)")))
 ;; (org-todo-keyword-faces '(("TODO" (:foreground "orange" :weight bold))
 ;;                           ("NEXT" (:foreground "red" :weight bold))
 ;;                           ("WIP" (:foreground "green" :weight bold))
 ;;                           ("DONE" (:foreground "gray"))))
 org-agenda-files '(org-directory
                    (expand-file-name "TODO.org" org-directory)))

;; (use-package ox-hugo
;;   :after ox)

;; Calendar and Journal

(require 'calendar)

(defun calendar-iso8601-date-string (date)
  "Create an ISO8601 date string from DATE."
  (destructuring-bind (month day year) date
    (concat (format "%4i" year)
            "-"
            (format "%02i" month)
            "-"
            (format "%02i" day))))

(defun calendar-date-add-days (date days)
  "Add DAYS to DATE."
  (calendar-gregorian-from-absolute
   (+ (calendar-absolute-from-gregorian date)
      days)))

(defun calendar-choose-date ()
  "Interactively choose DATE and return it as an ISO 8601 string."
  (let* ((today (calendar-current-date))
         (day-offsets '(0 -1 -2 -3 -4 -5 -6 -7))
         (dates (mapcar (apply-partially #'calendar-date-add-days today) day-offsets))
         (date-strings (mapcar #'calendar-iso8601-date-string dates)))
    (completing-read "Date: " date-strings nil nil (substring (car date-strings) 0 7))))

;;;###autoload
(defun calendar-insert-date (date)
  "Interactively choose a DATE in ISO 8601 format and insert it at point."
  (interactive (list (calendar-choose-date)))
  (insert date))

;;;###autoload
(defun calendar-insert-date-today ()
  "Insert today's date in ISO 8601 format."
  (interactive)
  (insert (calendar-iso8601-date-string (calendar-current-date))))

;;;###autoload
(defun journal-new-entry ()
  "Create a new journal entry."
  (interactive)
  (let ((date (calendar-choose-date)))
    (find-file (expand-file-name (concat date ".md") journal-directory))
    (if (= 0 (buffer-size))
        (progn
          (insert "journal")
          (yas-expand)))))

(provide 'm-org)

;;; m-org.el ends here
