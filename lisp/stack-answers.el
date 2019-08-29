;;; stack-answers.el --- Quick Stack Exchange Answers in Emacs -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: none
;; Keywords: reference, docs


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

;; Get quick answers to questions without leaving Emacs, using Stack Exchange.
;; Based on the idea from:
;; https://www.reddit.com/r/emacs/comments/cs6cb4/instant_stackoverflow_solutions_in_emacs_without/

;;;; TODO:
;; - Hide entire section, not just the block
;; - org-cycle type section cycling
;; - Replace unicode sequences in all shr rendered regions. Need to figure out
;;how to replace text and preserve text properties.
;; - The first request never completes...?

;;; Code:

(require 'xml)


;;;; Variables

(defcustom stack-answers-number-of-answers 5
  "The number of answers to show."
  :group 'stack-answers
  :type 'number)

(defcustom stack-answers-questions-function #'stack-answers--questions-sx
  "The function to call to retrieve question IDs.

This should be a list of strings. The strings are 8 digit
question IDs, like so:

\(\"1787996\" \"28100593\" \"613183\" \"4493205\" \"43477069\"\)"
  :group 'stack-answers
  :type 'symbol)

(defcustom stack-answers-sites '("stackexchange.com"
                                 "stackoverflow.com"
                                 "superuser.com")
  "The sites to search."
  :group 'stack-answers
  :type 'list)

;; Stolen from magit (`magit-section')
(defcustom stack-answers-section-visibility-indicator
  (if (window-system)
      '(magit-fringe-bitmap> . magit-fringe-bitmapv)
    '("…" . t))
  "Whether and how to indicate that a section can be expanded/collapsed.

If nil, then don't show any indicators.
Otherwise the value has to have one of these two forms:

(EXPANDABLE-BITMAP . COLLAPSIBLE-BITMAP)

  Both values have to be variables whose values are fringe
  bitmaps.  In this case every section that can be expanded or
  collapsed gets an indicator in the left fringe.

  To provide extra padding around the indicator, set
  `left-fringe-width' in `stack-answers-mode-hook'.

(STRING . BOOLEAN)

  In this case STRING (usually an ellipsis) is shown at the end
  of the heading of every collapsed section.  Expanded sections
  get no indicator.  The cdr controls whether the appearance of
  these ellipsis take section highlighting into account.  Doing
  so might potentially have an impact on performance, while not
  doing so is kinda ugly."
  :group 'stack-answers
  :type 'list)

(define-fringe-bitmap 'stack-answers-fringe-bitmap>
  [#b01100000
   #b00110000
   #b00011000
   #b00001100
   #b00011000
   #b00110000
   #b01100000
   #b00000000])

(define-fringe-bitmap 'stack-answers-fringe-bitmapv
  [#b00000000
   #b10000010
   #b11000110
   #b01101100
   #b00111000
   #b00010000
   #b00000000
   #b00000000])

(defvar stack-answers--query-history nil
  "List of previous queries.")

(defvar stack-answers-question-link-regexp
  "https://\\(superuser\\|stackoverflow\\|[a-z]+\\.stackexchange\\)\\.com/questions/\\([0-9]+\\)/"
  "Regexp to match a question link.")

(defvar stack-answers-current-query nil
  "The query for the current buffer.")
(make-variable-buffer-local 'stack-answers-current-query)

(defface stack-answers-query '((t (:inherit org-document-title)))
  "Face for the stackexchange answers query."
  :group 'stack-answers)

(defface stack-answers-question '((t (:inherit org-level-1)))
  "Face for a stackexchange answers question."
  :group 'stack-answers)

(defface stack-answers-answer '((t (:inherit org-level-2)))
  "Face for a stackexchange answers answer."
  :group 'stack-answers)

(defface stack-answers-comments '((t (:inherit org-level-3)))
  "Face for a stackexchange answers comment."
  :group 'stack-answers)

(defface stack-answers-block '((t (:inherit 'org-block)))
  "Face for a stackexchange answers block."
  :group 'stack-answers)

(define-button-type 'stack-answers-query
  'action #'stack-answers--query-action 'face 'stack-answers-query)

(define-button-type 'stack-answers-question
  'action #'shr-browse-url 'face 'stack-answers-question)

(define-button-type 'stack-answers-answer
  'action #'shr-browse-url 'face 'stack-answers-answer)

(define-button-type 'stack-answers-comments
  'action #'shr-browse-url 'face 'stack-answers-comments)


;;;; Functions
(defmacro stack-answers--http-get (url body)
  "Retrieve URL.

If the HTTP status code equals STATUS, evaluate BODY with point
at the start of the response body. Otherwise, throw a
`user-error'.

If nil, STATUS is 200.

ERROR-MESSAGE is the prefix to use for the, um, error message
when STATUS is not what is expected."
  `(with-current-buffer (url-retrieve-synchronously ,url)
     (goto-char (point-min))
     (re-search-forward " \\([0-9]+\\) " (line-end-position))
     (if (string= "200" (match-string-no-properties 1))
         (progn
           (goto-char (point-min))
           (search-forward "\n\n")
           ,body)
       (user-error "Error retrieving page: %s"
                   (buffer-substring-no-properties (point-min) (line-end-position))))))

(defun stack-answers--format-query-and-sites (query)
  "Format QUERY and `stack-answers-sites' for search engines."
  (url-hexify-string
   (format "%s (%s)" query
           (string-join (mapcar (apply-partially #'concat "site:")
                                stack-answers-sites)
                        " OR "))))

(defun stack-answers--question-id ()
  "Return a question-id element."
  (cons
   (replace-regexp-in-string "\\.stackexchange\\'" "" (match-string-no-properties 1))
   (match-string-no-properties 2)))

(defun stack-answers--questions-sx (query)
  "Send QUERY to stackexchange and return an alist.

Alist elements are:
\(site . question-id)

Where site is a string like \"stackexchange\" and question-id is
a number."
  (stack-answers--http-get
   (concat "https://stackexchange.com/search?q=" (url-hexify-string query))
   (cl-loop for i below stack-answers-number-of-answers
            while (and (re-search-forward "result-link" nil t)
                       (re-search-forward stack-answers-question-link-regexp nil t))
            collect (stack-answers--question-id))))

(defun stack-answers--get-questions-from-google (query)
  "Send QUERY to google and return a list of IDs as strings.

See `stack-answers--get-questions-from-sx' for details."
  (stack-answers--http-get
   (concat "https://google.com/search?q="
           (stack-answers--format-query-and-sites query))
   (cl-loop for i below stack-answers-number-of-answers
            while (re-search-forward stack-answers-question-link-regexp nil t)
            collect (stack-answers-question-id))))

(defun stack-answers--get-questions-from-ddg (query)
  "Send QUERY to DuckDuckGo and return a list of IDs as strings.

See `stack-answers--get-questions-from-sx' for details."
  (stack-answers--http-get
   (concat "https://duckduckgo.com/html/?q="
           (stack-answers--format-query-and-sites query))
   (cl-loop for i below stack-answers-number-of-answers
            while (re-search-forward
                   (url-hexify-string stack-answers-question-link-regexp)
                   nil t)
            collect (stack-answers-question-id))))

(defun stack-answers--read-query (&optional query)
  "Prompt the user, using QUERY as the initial input.

PROMPT is the same as for `completing-read'."
  (completing-read "Query: " nil nil nil query stack-answers--query-history))

(defun stack-answers--query-action (button)
  "Re-run `stack-answers' with and updated query from BUTTON."
  (stack-answers
   (stack-answers--read-query
    (buffer-substring-no-properties (button-start button) (button-end button)))))

(defun stack-answers--group-site-questions (question-ids)
  "Group QUESTION-IDS by site."
  (cl-loop for site in (cl-remove-duplicates (mapcar #'car question-ids)
                                             :test #'string=)
           collect
           (cons site (cl-remove-duplicates
                       (cl-loop for q in question-ids
                                when (string= site (car q))
                                collect (cdr q))
                       :test #'string=))))

(defun stack-answers--get-questions (question-ids)
  "Given a list of QUESTION-IDS, return question objects.

See `stack-answers--get-questions-from-sx' for the QUESTION-IDS format."
  (assoc-default
   'items
   (car
    (cl-loop for (site . ids) in
             (stack-answers--group-site-questions question-ids)
             collect
             (stack-answers--http-get
              ;; https://api.stackexchange.com/docs/questions-by-ids#pagesize=5&order=desc&sort=votes&ids=34223197%3B34032558%3B15385099%3B45282799%3B36879031&filter=!BPXMTrCHygViBvgpVRCp*YeL4fTt*u&site=stackoverflow&run=true
              (format "https://api.stackexchange.com/2.2/questions/%s?pagesize=5&order=desc&sort=votes&site=%s&filter=!BPXMTrCHygViBvgpVRCp*YeL4fTt*u"
                      (string-join ids ";")
                      site)
              (json-read))))))

(defun stack-answers--sort-answers (answers)
  "Sort the ANSWERS returned by the API."
  (seq-sort (lambda (a b) (>= (assoc-default 'up_vote_count a)
                              (assoc-default 'up_vote_count b)))
            answers))

(defun stack-answers-next-question ()
  "Move point to the next question."
  (interactive)
  (while (and (forward-button 1)
              (not (equal (button-type (button-at (point)))
                          'stack-answers-question)))))

(defun stack-answers-previous-question ()
  "Move point to the next question."
  (interactive)
  (while (and (forward-button -1)
              (not (equal (button-type (button-at (point)))
                          'stack-answers-question)))))

(defun stack-answers-toggle-display ()
  "Toggle display of the section at point."
  (interactive)
  (save-excursion
    (when-let* ((b (or (button-at (point)) (button-at (1+ (point)))))
                (type (button-get b 'type))
                (pos (next-overlay-change (point))))
      ;; `next-overlay-change' returns `(point-max)' when no overlay is found.
      (while (not (= pos (point-max)))
        (if-let ((o (cl-some
                     (lambda (o) (when (equal type (overlay-get o 'type)) o))
                     (overlays-at pos))))
            (let ((invisible (overlay-get o 'invisible)))
              (overlay-put o 'invisible (if invisible nil type))
              (overlay-put o 'after-string 'stack-answers-fringe-bitmap>)
              (overlay-put (make-overlay (button-start b) (button-end b))
                           'before-string
                           (propertize
                            "fringe" 'display
                            `(left-fringe
                              ,(if invisible
                                   (cdr stack-answers-section-visibility-indicator)
                                 (car stack-answers-section-visibility-indicator))
                              ,(face-foreground 'fringe))))
              (setq pos (point-max)))
          (setq pos (next-overlay-change pos)))))))

(defvar stack-answers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'forward-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "M-n") #'forward-button)
    (define-key map (kbd "M-p") #'backward-button)
    (define-key map (kbd "C-c C-n") #'stack-answers-next-question)
    (define-key map (kbd "C-c C-p") #'stack-answers-previous-question)
    (define-key map (kbd "<tab>") #'stack-answers-toggle-display)
    map)
  "Keymap for `stack-answers-mode'.")

(define-derived-mode stack-answers-mode help-mode "stackexchange answers"
  "Display stackexchange answers in Emacs."
  :group 'help
  (set (make-local-variable 'buffer-read-only) t)
  (setq revert-buffer-function
        (lambda (&rest _) (stack-answers stack-answers-current-query))))

(defun stack-answers-insert-comments (comments)
  "Insert COMMENTS section."
  (when comments
    (insert-text-button "⋯" 'type 'stack-answers-comments)
    (overlay-put (make-overlay (point-at-bol) (point))
                 'before-string
                 (propertize "fringe" 'display
                             `(left-fringe
                               stack-answers-fringe-bitmapv
                               ,(face-foreground 'fringe))))
    (let ((comments-start (point)))
      (seq-doseq (c comments)
        (insert (propertize (concat "\n    " (assoc-default 'body c)) 'face 'shadow)))
      (let ((o (make-overlay comments-start (point))))
        (overlay-put o 'type 'stack-answers-comments)
        (overlay-put o 'invisible nil)))
    (insert "\n")))

(defun stack-answers-insert-answer (answer)
  "Insert ANSWER section."
  (insert-text-button "Answer"
                      'type 'stack-answers-answer
                      'shr-url (assoc-default 'link answer)
                      'help-echo (assoc-default 'link answer))
  (overlay-put (make-overlay (point-at-bol) (point))
               'before-string
               (propertize "fringe" 'display
                           `(left-fringe
                             stack-answers-fringe-bitmapv
                             ,(face-foreground 'fringe))))
  (insert (if (equal (assoc-default 'is_accepted answer) t) " ✔" "")
          " ⬆"
          (number-to-string (assoc-default 'up_vote_count answer))
          "\n")
  (let ((answer-start (point)))
    (insert (assoc-default 'body answer))
    (shr-render-region answer-start (point))
    (overlay-put (make-overlay answer-start (point))
                 'face `(:background ,(face-background 'org-block)))
    (stack-answers-insert-comments (assoc-default 'comments answer))
    (let ((o (make-overlay answer-start (point))))
      (overlay-put o 'type 'stack-answers-answer)
      (overlay-put o 'invisible nil))
    (insert "\n")))

(defun stack-answers-insert-question (question)
  "Insert QUESTION section."
  (insert-text-button (replace-regexp-in-string xml-char-ref-re
                                                #'xml--entity-replacement-text
                                                (assoc-default 'title question))
                      'type 'stack-answers-question
                      'shr-url (assoc-default 'link question)
                      'help-echo (assoc-default 'link question))
  (overlay-put (make-overlay (point-at-bol) (point))
               'before-string
               (propertize "fringe" 'display
                           `(left-fringe
                             stack-answers-fringe-bitmapv
                             ,(face-foreground 'fringe))))

  (insert " ⬆"
          (number-to-string (assoc-default 'up_vote_count question))
          "\n")

  (let* ((question-start (point)))
    (insert (assoc-default 'body question))
    (shr-render-region question-start (point))
    
    (insert "\n")
    (seq-do #'stack-answers-insert-answer (assoc-default 'answers question))
    
    (let ((o (make-overlay question-start (point))))
      (overlay-put o 'type 'stack-answers-question)
      (overlay-put o 'invisible nil))))


;;;; Commands

;;;###autoload
(defun stack-answers (query)
  "Get answers to QUERY via stackoverflow."
  (interactive (list (stack-answers--read-query)))
  (if-let ((window (get-buffer-window "*stackexchange*")))
      (select-window window)
    (switch-to-buffer-other-window "*stackexchange*"))

  (let ((inhibit-read-only t))
    (erase-buffer)
    (remove-overlays)

    (insert (make-text-button query nil 'type 'stack-answers-query
                              'help-echo (format "Change query [%s]" query))
            (propertize "\n\n" 'face 'default))
    
    (seq-do
     #'stack-answers-insert-question
     (stack-answers--get-questions (funcall stack-answers-questions-function query)))

    (goto-char (point-min))
    (while (search-forward "" nil t)
      (replace-match ""))
    (goto-char (point-min)))
    ;; TODO: Not sure if this is needed
    ;; (while (re-search-forward xml-char-ref-re nil t)
    ;;   (let* ((beg (match-beginning 0))
    ;;          (end (match-end 0))
    ;;          (entity (buffer-substring-no-properties beg end)))
    ;;     (delete-region beg end)
    ;;     (insert-and-inherit (xml--entity-replacement-text entity)))))
  (goto-char (point-min))
  (stack-answers-mode)
  (setq stack-answers-current-query query))

(provide 'stack-answers)

;;; stack-answers.el ends here
