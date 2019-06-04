;;; m-notes.el --- Note taking -*- lexical-binding: t -*-

;;; Commentary:

;; Note taking and Org

;;; Code:

;; visual-line-mode
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

;;;; Org

(defun search-org-files ()
  "Search ~/org using `counsel-rg'."
  (interactive)
  (counsel-rg nil org-directory))

(defun org-todo-todo ()
  "Create or update Org todo entry to TODO status."
  (interactive)
  (org-todo "TODO"))

(defun org-todo-to-int (todo)
  "Get the number of the TODO based on its status."
  (first (cl-remove nil
                    (mapcar (lambda (keywords)
                              (let ((todo-seq
                                     (mapcar (lambda (x) (first (split-string  x "(")))
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

(defun org-sort-entries-by-todo-status ()
  "Sort Org TODO entries by their status."
  (interactive)
  (org-sort-entries nil ?f #'org-sort-entries--todo-status-key))

(defun org-archive-done-tasks-in-file ()
  "Archive all tasks marked done."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

(use-package org
  :straight
  (:type built-in)
  :custom
  ( org-directory "~/org")
  ;; Clean view
  (org-startup-indented t)
  ;; Smart C-a/e
  (org-special-ctrl-a/e t)
  ;; Smart C-k
  (org-special-ctrl-k t)
  ;; Insert a row in tables
  (org-special-ctrl-o t)
  ;; Tab in source blocks should act like in major mode
  (org-src-tab-acts-natively t)
  ;; Code highlighting in code blocks
  (org-src-fontify-natively t)
  ;; Customize todo keywords
  ;; (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WIP(w)" "|" "DONE(d!)")))
  ;; (org-todo-keyword-faces '(("TODO" (:foreground "orange" :weight bold))
  ;;                           ("NEXT" (:foreground "red" :weight bold))
  ;;                           ("WIP" (:foreground "green" :weight bold))
  ;;                           ("DONE" (:foreground "gray"))))
  (org-agenda-files '(org-directory (expand-file-name "TODO.org" org-directory)))
  (org-catch-invisible-edits 'show-and-error)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-switchb)
   ("C-c s" . search-org-files)
   ("C-c n" . (lambda () (interactive) (find-file (expand-file-name "new-note.org"))))
   ("C-c o" . (lambda () (interactive) (find-file org-directory)))
   :map org-mode-map
   ("s-;" . org-shiftright)
   :map visual-line-mode-map
   ;; Don't shadow mwim and org-mode bindings
   ([remap move-beginning-of-line] . nil)))

(use-package org-capture
  :straight
  (:type built-in)
  :config
  (add-to-list 'org-capture-templates
               '("m"
                 "TODO respond to email"
                 entry
                 (file (expand-file-name "TODO.org" org-directory))
                 "* TODO %^{Description}\n%A\n%?\n"))
  :commands
  (org-capture org-capture-refile))

;; (use-package ox-hugo
;;   :after ox)

;;;; Calendar and Journal

(use-package calendar
  :commands
  (calendar-gregorian-from-absolute))

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
  (calendar-gregorian-from-absolute (+ (calendar-absolute-from-gregorian date) days)))

(defun calendar-choose-date ()
  "Interactively choose DATE and return it as an ISO 8601 string."
  (let* ((today (calendar-current-date))
         (day-offsets '(0 -1 -2 -3 -4 -5 -6 -7))
         (dates (mapcar (apply-partially #'calendar-date-add-days today) day-offsets))
         (date-strings (mapcar #'calendar-iso8601-date-string dates)))
    (completing-read "Date: " date-strings nil nil (substring (car date-strings) 0 7))))

(defun calendar-insert-date (date)
  "Interactively choose a DATE in ISO 8601 format and insert it at point."
  (interactive (list (calendar-choose-date)))
  (insert date))

(defun calendar-insert-date-today ()
  "Insert today's date in ISO 8601 format."
  (interactive)
  (insert (calendar-iso8601-date-string (calendar-current-date))))

(defun journal-new-entry ()
  "Create a new journal entry."
  (interactive)
  (let ((date (calendar-choose-date)))
    (find-file (expand-file-name (concat date ".md") journal-directory))
    (if (= 0 (buffer-size))
        (progn
          (insert "journal")
          (yas-expand)))))

;;;; Reading

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :hook
  (after-init . pdf-loader-install)
  (pdf-view-mode . (lambda () (auto-revert-mode -1)))
  :bind
  (:map pdf-view-mode-map
        ("s-f" . isearch-forward)))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(provide 'm-notes)

;;; m-notes.el ends here
