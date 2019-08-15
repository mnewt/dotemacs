;;; note.el --- Note taking -*- lexical-binding: t -*-

;;; Commentary:

;; Notes, to dos, reading, Org

;;; Code:

;;;; Org

;; Make package.el install Org from repo instead of using the built in version.
(assq-delete-all 'org package--builtins)
(unless (file-expand-wildcards (concat package-user-dir "/org-[0-9]*"))
 (package-install (elt (cdr (assoc 'org package-archive-contents)) 0)))
;; We have to be really sure something doesn't load `org' before this, or we get
;; the version that ships with Emacs.

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-directory "~/org")
  ;; Indent text according to the outline structure.
  (org-startup-indented t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  ;; Insert a row in tables
  (org-special-ctrl-o t)
  ;; Quit adding 2 spaces to source block
  (org-edit-src-content-indentation 0)
  ;; Tab in source blocks should act like in major mode
  (org-src-tab-acts-natively t)
  ;; Code highlighting in code blocks
  (org-src-fontify-natively t)
  (org-hide-leading-stars t)
  (org-export-with-section-numbers nil)
  ;; Customize todo keywords
  (org-todo-keywords '((sequence "TODO(t)" "WIP(w)" "DONE(d!)")))
  (org-todo-keyword-faces '(("TODO" (:foreground "magenta" :weight bold))
                            ("WIP" (:foreground "hot pink" :weight bold))
                            ("DONE" (:foreground "gray" :weight bold))))
  (org-catch-invisible-edits 'show-and-error)
  (org-capture-templates
   `(("t" "TODO" entry
      (file+headline ,(expand-file-name "TODO.org" org-directory) "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("n" "Note" entry
      (file+headline ,(expand-file-name "TODO.org" org-directory) "Tasks")
      "* %?\n  %i\n  %a")
     ("m" "TODO respond to email" entry
      (file ,(expand-file-name "TODO.org" org-directory))
      "* TODO %^{Description}\n%A\n%?\n")))
  ;; Don't prompt to confirm if I want to evaluate a source block
  (org-confirm-babel-evaluate nil)
  (org-startup-with-inline-images "inlineimages")
  (org-image-actual-width 500)
  :commands
  org-todo
  org-entry-get
  org-sort-entries
  org-map-entries
  org-capture
  org-capture-refile
  :config

  (defun org-search-org-directory ()
    "Search ~/org using `counsel-rg'."
    (interactive)
    (let ((default-directory org-directory))
      (counsel-rg)))

  (defun org-todo-todo ()
    "Create or update Org todo entry to TODO status."
    (interactive)
    (org-todo "TODO"))

  (defun org-todo-to-int (todo)
    "Get the number of the TODO based on its status."
    (car (cl-remove
          nil
          (mapcar (lambda (keywords)
                    (let ((todo-seq
                           (mapcar (lambda (x) (car (split-string  x "(")))
                                   (cdr keywords))))
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

  (defface org-emphasis-marker-face '((t (:inherit shadow)))
    "Face for Org emphasis markers"
    :group 'org-faces)

  ;; This is a re-definition of a built in function.
  ;; TODO Follow up with Org mailing list on this approach.
  (defun org-do-emphasis-faces (limit)
    "Run through the buffer and emphasize strings."
    (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\)"
                            (car org-emphasis-regexp-components))))
      (catch :exit
        (while (re-search-forward quick-re limit t)
          (let* ((marker (match-string 2))
                 (verbatim? (member marker '("~" "="))))
            (when (save-excursion
                    (goto-char (match-beginning 0))
                    (and
                     ;; Do not match table hlines.
                     (not (and (equal marker "+")
                               (org-match-line
                                "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
                     ;; Do not match headline stars.  Do not consider
                     ;; stars of a headline as closing marker for bold
                     ;; markup either.
                     (not (and (equal marker "*")
                               (save-excursion
                                 (forward-char)
                                 (skip-chars-backward "*")
                                 (looking-at-p org-outline-regexp-bol))))
                     ;; Match full emphasis markup regexp.
                     (looking-at (if verbatim? org-verbatim-re org-emph-re))
                     ;; Do not span over paragraph boundaries.
                     (not (string-match-p org-element-paragraph-separate
                                          (match-string 2)))
                     ;; Do not span over cells in table rows.
                     (not (and (save-match-data (org-match-line "[ \t]*|"))
                               (string-match-p "|" (match-string 4))))))
              (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist)))
                (font-lock-prepend-text-property
                 (match-beginning 2) (match-end 2) 'face face)
                (when verbatim?
                  (org-remove-flyspell-overlays-in
                   (match-beginning 0) (match-end 0))
                  (remove-text-properties (match-beginning 2) (match-end 2)
                                          '(display t invisible t intangible t)))
                (add-text-properties (match-beginning 2) (match-end 2)
                                     '(font-lock-multiline t org-emphasis t)))

              (font-lock-prepend-text-property
               (match-beginning 3) (match-end 3) 'face 'org-emphasis-marker-face)
              (font-lock-prepend-text-property
               (match-end 4) (match-beginning 5) 'face 'org-emphasis-marker-face)

              (when org-hide-emphasis-markers
                (add-text-properties (match-end 4) (match-beginning 5)
                                     '(invisible org-link))
                (add-text-properties (match-beginning 3) (match-end 3)
                                     '(invisible org-link))))
            (throw :exit t))))))

  (use-package org-download
    :after org
    :hook
    (dired-mode-hook . org-download-enable))

  ;; Required for Org html export
  (use-package htmlize
    :commands
    htmlize-file
    htmlize-region
    htmlize-buffer
    htmlize-many-files
    htmlize-many-files-dired
    org-html-htmlize-generate-css)

  (use-package org-preview-html
    :commands
    org-preview-html-mode)

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-switchb)
   :map m-map
   ("s" . org-search-org-directory)
   ("n" . (lambda () (interactive)
            (find-file (expand-file-name "new-note.org"))))
   ("o" . (lambda () (interactive) (find-file org-directory)))
   :map org-mode-map
   ("C-M-}" . org-forward-sentence)
   ("C-M-{" . org-backward-sentence)
   ("M-S-<up>" . org-move-subtree-up)
   ("M-S-<down>" . org-move-subtree-down)
   ("s->" . org-shiftright)
   ("s-<" . org-shiftleft)
   :map visual-line-mode-map
   ;; Don't shadow mwim and org-mode bindings
   ([remap move-beginning-of-line] . nil)))

(use-package poporg
  :bind
  ("C-c C-'" . poporg-dwim))

;; Requires Org 9.3, which I'm not using yet.
;; (use-package orglink
;;   :after org
;;   :hook
;;   (prog-mode-hook . orglink-mode))

;;;; Calendar and Journal

(use-package calendar
  :commands
  calendar-current-date
  :config
  (defun calendar-iso8601-date-string (date)
    "Create an ISO8601 date string from DATE."
    (cl-destructuring-bind (month day year) date
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

  (defvar journal-directory)

  (defun journal-new-entry ()
    "Create a new journal entry."
    (interactive)
    (let ((date (calendar-choose-date)))
      (find-file (expand-file-name (concat date ".md") journal-directory))
      (if (= 0 (buffer-size))
          (progn
            (insert "journal")
            (yas-expand)))))

  :commands
  (calendar-gregorian-from-absolute
   new-journal-entry
   calendar-insert-date
   calendar-choose-date)
  :bind
  ("M-m i d" . calendar-insert-date)
  ("M-m i t" . calendar-insert-date-today)
  ("M-m j" . journal-new-entry))

;;;; Reading

(defun brew-prefix (package)
  "Get the `homebrew' install prefix for PACKAGE."
  (shell-command-to-string (format "printf %%s \"$(brew --prefix %s)\"" package)))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; Might need to download, make, and install poppler from source.

  (let ((orig (getenv "PKG_CONFIG_PATH")))
    (setenv "PKG_CONFIG_PATH"
            (concat "" orig
                    ":" (brew-prefix "poppler") "/lib/pkgconfig"
                    ":" (brew-prefix "libffi") "/lib/pkgconfig"
                    ":" (brew-prefix "glib") "/lib/pkgconfig"
                    ":" (brew-prefix "pcre") "/lib/pkgconfig"
                    ":" (brew-prefix "libpng") "/lib/pkgconfig"))
    (pdf-loader-install)
    (setenv "PKG_CONFIG_PATH" orig))
  :bind
  (:map pdf-view-mode-map
        ("s-f" . isearch-forward)))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(provide 'm-note)

;;; note.el ends here
