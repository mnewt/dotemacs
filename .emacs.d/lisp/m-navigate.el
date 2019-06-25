;;; m-navigation.el --- Navigation Functions -*- lexical-binding: t -*-

;;; Commentary:

;; Navigation functions

;;; Code:

(use-package dashboard
  :defer 0.5
  :custom
  (dashboard-center-content t)
  (dashboard-set-footer nil)
  (dashboard-items '((recents  . 8)
                     (projects . 8)
                     (registers . 8)
                     (bookmarks . 8)))
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   `((("*" "Org Files" "~/org"
       (lambda (&rest _) (dired "~/org")))
      ("⚙" "Emacs Config" "dotemacs"
       (lambda (&rest _) (dired-list-init-files)))
      ("~" "Home Directory" "dotfiles"
       (lambda (&rest _) (dired "~")))
      ("↓" "Code Directory" "~/code"
       (lambda (&rest _) (dired "~/code")))
      ("↑" "Dropbox Code" "~/Dropbox/code"
       (lambda (&rest _) (dired "~/Dropbox/Matt/code")))
      ("?" "Info" "?/h"
       (lambda (&rest _) (info) (delete-other-windows))))))
  :config
  (dashboard-insert-shortcut "j" "Projects:")
  (dashboard-insert-startupify-lists)
  (switch-to-buffer "*dashboard*")
  (dashboard-mode)
  (goto-char (point-min))
  (dashboard-next-section)
  ;; :hook
  ;; (after-init . #'dashboard-insert-startupify-lists)
  ;; (emacs-startup . (lambda ()
  ;;                    (switch-to-buffer "*dashboard*")
  ;;                    (dashboard-mode)
  ;;                    (goto-char (point-min))
  ;;                    (dashboard-next-line 1)))
  :bind
  (:map dashboard-mode-map
        ("p" . dashboard-previous-line)
        ("n" . dashboard-next-line)))

(use-package evil
  :init
  (defun evil-mode-toggle ()
    "Toggle `evil-mode'.
It's necessary because it often forgets to change the cursor type back."
    (interactive)
    (if (bound-and-true-p evil-state)
        (progn
          (call-interactively #'turn-off-evil-mode)
          (setq cursor-type 'box))
      (call-interactively #'turn-on-evil-mode)))
  :commands
  (turn-on-evil-mode turn-off-evil-mode)
  :bind
  ("s-ESC" . evil-mode-toggle)
  ("s-<escape>" . evil-mode-toggle))

(defun fullscreen ()
  "Toggle fullscreen mode."
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun previous-line-margin ()
  "Move point to the top of the window.

If it's already there, scroll `scroll-margin' lines up."
  (interactive)
  (let ((line (line-number-at-pos))
        (line-beg (line-number-at-pos (window-start))))
    (if (= (- line line-beg) scroll-margin)
        (forward-line (- scroll-margin))
      (forward-line (+ (- line-beg line) scroll-margin)))))

(defun next-line-margin ()
  "Move point to the bottom of the window.

If it's already there, scroll `scroll-margin' lines down."
  (interactive)
  (let ((line (line-number-at-pos))
        (line-end (- (line-number-at-pos (window-end)) 2)))
    (if (= (- line-end line) scroll-margin)
        (forward-line scroll-margin)
      (forward-line (- line-end line scroll-margin)))))

(defvar scratch-other-modes
  '(lisp-interaction-mode js-mode js-jsx-mode)
  "Modes to add to the new scratch buffer list.
This list exists because these modes may not be added
  automatically. See `list-major-modes'.")

(defun filter-buffers-by-name (regexp)
  "Return a list of buffers whose names match REGEXP."
  (seq-filter (lambda (b) (string-match-p regexp (buffer-name b)))
              (buffer-list)))

(defun filter-buffers-by-mode (mode)
  "Return a list of buffers whose major mode is MODE."
  (when (stringp mode) (setq mode (intern mode)))
  (seq-filter (lambda (b) (eq (buffer-local-value 'major-mode b) mode))
              (buffer-list)))

(defun list-buffer-major-modes ()
  "Return a list of all major modes currently in use in open buffers."
  (delete-dups (mapcar (lambda (b) (buffer-local-value 'major-mode b))
                       (buffer-list))))

(defun list-major-modes ()
  "Return a list of all major modes.

It actually does not list them all because I don't know how to do
  that. So, we find only ones which are associated with a magic
  string or file extension."
  (delete-dups (mapcar #'cdr (append magic-mode-alist
                                     auto-mode-alist
                                     magic-fallback-mode-alist))))

(defun scratch-new-buffer ()
  "Create or go to a scratch buffer in the current mode.

If ARG is provided then prompt for the buffer's mode. Try these
  things in succession\:

1. Select an existing window containing the scratch buffer.
2. Switch to an existing scratch buffer.
3. Create a new scratch buffer and switch to it."
  (interactive)
  (let* ((mode (if current-prefix-arg
                   (intern (ivy-read "New scratch buffer with mode: "
                                     (append scratch-other-modes (list-major-modes))
                                     :history 'new-scratch-buffer-history
                                     :caller 'scratch-new-buffer))
                 ;; :initial-input (car new-scratch-buffer-history)))
                 major-mode))
         (name (format "<%s>" (symbol-name mode)))
         (win (get-buffer-window name)))
    (cond
     (win (select-window win))
     (t (switch-to-buffer (get-buffer-create name))
        (setq buffer-file-name name)
        (funcall mode)))))

(defun scratch-new-buffer-other-window ()
  "Create or go to a scratch buffer in ther current mode.

For for details see `scratch-new-buffer'."
  (interactive)
  (switch-to-buffer-other-window (current-buffer))
  (scratch-new-buffer))

(defun switch-to-buffer-by-mode (mode)
  "Interactively choose a major MODE, then choose a buffer of that mode."
  (interactive
   (list (ivy-read "Choose buffers for major mode: "
                   (list-buffer-major-modes)
                   :history 'switch-to-buffer-by-mode-history
                   :action 'switch-to-buffer-by-mode)))
  (when (stringp mode) (setq mode (intern mode)))
  (let ((buffers (mapcar #'buffer-name (filter-buffers-by-mode mode))))
    (ivy-read (format "%s buffers: " mode) buffers
              :keymap ivy-switch-buffer-map
              :action #'ivy--switch-buffer-action
              :matcher #'ivy--switch-buffer-matcher
              :preselect (when (eq major-mode mode) (cadr buffers))
              ;; Use the `ivy-switch-buffer' actions.
              :caller #'ivy-switch-buffer)))

;; kill buffer and window
(defun kill-other-buffer-and-window ()
  "Kill the buffer in the other window."
  (interactive)
  (select-window (next-window))
  (kill-buffer-and-window))

;; https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  "Toggle windows between horizontal and vertical split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun find-file-at-point-with-line (&optional filename)
  "Open FILENAME at point and move point to line specified next to file name."
  (interactive)
  (let* ((filename (or filename (if current-prefix-arg (ffap-prompter) (ffap-guesser))))
         (line-number
          (and (or (looking-at ".* line \\(\[0-9\]+\\)")
                   (looking-at "[^:]*:\\(\[0-9\]+\\)"))
               (string-to-number (match-string-no-properties 1))))
         (column-number
          (or
           (and (looking-at "[^:]*:\[0-9\]+:\\(\[0-9\]+\\)")
                (string-to-number (match-string-no-properties 1)))
           0)))
    (message "%s --> %s:%s" filename line-number column-number)
    (cond ((ffap-url-p filename)
           (let (current-prefix-arg)
             (funcall ffap-url-fetcher filename)))
          ((and line-number
                (file-exists-p filename))
           (progn (find-file-other-window filename)
                  (goto-char (point-min))
                  (forward-line (1- line-number))
                  (forward-char column-number)))
          ((and ffap-pass-wildcards-to-dired
                ffap-dired-wildcards
                (string-match ffap-dired-wildcards filename))
           (funcall ffap-directory-finder filename))
          ((and ffap-dired-wildcards
                (string-match ffap-dired-wildcards filename)
                find-file-wildcards
                ;; Check if it's find-file that supports wildcards arg
                (memq ffap-file-finder '(find-file find-alternate-file)))
           (funcall ffap-file-finder (expand-file-name filename) t))
          ((or (not ffap-newfile-prompt)
               (file-exists-p filename)
               (y-or-n-p "File does not exist, create buffer? "))
           (funcall ffap-file-finder
                    ;; expand-file-name fixes "~/~/.emacs" bug sent by CHUCKR.
                    (expand-file-name filename)))
          ;; User does not want to find a non-existent file:
          ((signal 'file-error (list "Opening file buffer"
                                     "no such file or directory"
                                     filename))))))

(defun parse-colon-notation (filename)
  "Parse FILENAME in the format expected by `server-visit-files'.
Modify it so that `filename:line:column' is is reformatted the
way Emacs expects."
  (let ((name (car filename)))
    (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
        (cons
         (match-string 1 name)
         (cons (string-to-number (match-string 2 name))
               (string-to-number (or (match-string 3 name) ""))))
      filename)))

(defun wrap-colon-notation (f &rest args)
  "Wrap F (`server-visit-files') and modify ARGS to support colon notation.
Open files with emacsclient with cursors according to colon
notation. When the file name has line numbers and optionally
columns specified like `filename:line:column', parse those and
return them in the Emacs format."
  (message "%s" args)
  (apply f (cons (mapcar #'parse-colon-notation (car args)) (cdr args))))

;; Make `emacsclient' support solon notation of line:column
(advice-add 'server-visit-files :around #'wrap-colon-notation)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for line number N."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (forward-line)
        (let ((n (1- (read-number "Go to line: "))))
          (goto-char (point-min))
          (forward-line n)))
    (linum-mode -1)))

;; Show line in the original buffer from occur mode
(setq list-matching-lines-jump-to-current-line t)

(use-package winner
  :defer 5
  :config
  (winner-mode)
  :bind
  ("C-c [" . winner-undo)
  ("s-[" . winner-undo)
  ("C-c ]" . winner-redo)
  ("s-]" . winner-redo))

(use-package buffer-move
  :bind
  ("C-H-W" . buf-move-up)
  ("C-H-S" . buf-move-down)
  ("C-H-A" . buf-move-left)
  ("C-H-D" . buf-move-right))

(use-package winum
  :defer 3
  :custom
  (winum-auto-setup-mode-line nil)
  :config
  (winum-mode)
  :bind
  ("s-1" . winum-select-window-1)
  ("C-c 1" . winum-select-window-1)
  ("s-2" . winum-select-window-2)
  ("C-c 2" . winum-select-window-2)
  ("s-3" . winum-select-window-3)
  ("C-c 3" . winum-select-window-3)
  ("s-4" . winum-select-window-4)
  ("C-c 4" . winum-select-window-4)
  ("s-5" . winum-select-window-5)
  ("C-c 5" . winum-select-window-5)
  ("s-6" . winum-select-window-6)
  ("C-c 6" . winum-select-window-6)
  ("s-7" . winum-select-window-7)
  ("C-c 7" . winum-select-window-7)
  ("s-8" . winum-select-window-8)
  ("C-c 8" . winum-select-window-8)
  ("s-9" . winum-select-window-9)
  ("C-c 9" . winum-select-window-9)
  ("s-0" . winum-select-window-0)
  ("C-c 0" . winum-select-window-0))

(use-package eyebrowse
  :defer 2
  :custom
  (eyebrowse-new-workspace t)
  (eyebrowse-mode-line-separator " ")
  :config
  (eyebrowse-mode)
  :bind
  ("H-1" . eyebrowse-switch-to-window-config-1)
  ("C-c C-1" . eyebrowse-switch-to-window-config-1)
  ("H-2" . eyebrowse-switch-to-window-config-2)
  ("C-c C-2" . eyebrowse-switch-to-window-config-2)
  ("H-3" . eyebrowse-switch-to-window-config-3)
  ("C-c C-3" . eyebrowse-switch-to-window-config-3)
  ("H-4" . eyebrowse-switch-to-window-config-4)
  ("C-c C-4" . eyebrowse-switch-to-window-config-4)
  ("H-5" . eyebrowse-switch-to-window-config-5)
  ("C-c C-5" . eyebrowse-switch-to-window-config-5)
  ("H-6" . eyebrowse-switch-to-window-config-6)
  ("C-c C-6" . eyebrowse-switch-to-window-config-6)
  ("H-7" . eyebrowse-switch-to-window-config-7)
  ("C-c C-7" . eyebrowse-switch-to-window-config-7)
  ("H-8" . eyebrowse-switch-to-window-config-8)
  ("C-c C-8" . eyebrowse-switch-to-window-config-8)
  ("H-9" . eyebrowse-switch-to-window-config-9)
  ("C-c C-9" . eyebrowse-switch-to-window-config-9)
  ("H-0" . eyebrowse-switch-to-window-config-0)
  ("C-c C-0" . eyebrowse-switch-to-window-config-0))

;; Create friendly names for buffers with the same name
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; scratch
(setq initial-scratch-message nil
      initial-major-mode 'lisp-interaction-mode)

;; Try to re-use help buffers of different sorts
(setq display-buffer-alist
      `((,(rx bos
              (or "*Apropos*" "*eww*" "*Help*" "*helpful" "*info*" "*Summary*")
              (0+ not-newline))
         (display-buffer-reuse-mode-window display-buffer-pop-up-window)
         (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))

(with-eval-after-load 'ediff
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package goto-addr
  :hook
  ((compilation-mode text-mode eshell-mode shell-mode) . goto-address-mode)
  (prog-mode . goto-address-prog-mode)
  :bind
  (:map goto-address-highlight-keymap
        ("C-c C-o" . goto-address-at-point)))

(defun outline-show-current-sublevel ()
  "Show only the current top level section."
  (interactive)
  (unless (bound-and-true-p 'outline-minor-mode)
    (outline-minor-mode t))
  (outline-hide-sublevels 1)
  (outline-show-subtree))

(defun outline-subtree-previous ()
  "Go to and expand previous sublevel."
  (interactive)
  (unless (bound-and-true-p 'outline-minor-mode)
    (outline-minor-mode t))
  (outline-hide-sublevels 1)
  (outline-previous-visible-heading 1)
  (outline-show-subtree))

(defun outline-subtree-next ()
  "Go to and expand previous sublevel."
  (interactive)
  (unless (bound-and-true-p 'outline-minor-mode)
    (outline-minor-mode t))
  (outline-hide-sublevels 1)
  (outline-next-visible-heading 1)
  (outline-show-subtree))

;; (use-package outshine
;;   :defer 4
;;   :after outline
;;   :custom
;;   (outline-minor-mode-prefix "\M-#")
;; :config

;; (put 'narrow-to-region 'disabled t)
;;   (defun outshine-narrow-dwim (&rest _args)
;;     (unless (outline-on-heading-p t)
;;       (outline-previous-visible-heading 1)))

;;   ;; Narrowing now works within the headline rather than requiring to be on it
;;   (advice-add 'outshine-narrow-to-subtree :before #'outshine-narrow-dwim)

;;   (defhydra hydra-outline (:color pink :hint nil)
;;     "
;; Outline

;; ^Hide^             ^Show^           ^Move
;; ^^^^^^------------------------------------------------------
;; _s_ sublevels     _a_ all         _u_ up
;; _t_ body          _e_ entry       _n_ next visible
;; _o_ other         _i_ children    _p_ previous visible
;; _c_ entry         _k_ branches    _f_ forward same level
;; _l_ leaves        _s_ subtree     _b_ backward same level
;; _d_ subtree

;; "
;;     ;; Hide
;;     ("q" outline-hide-sublevels) ; Hide everything but the top-level headings
;;     ("t" outline-hide-body)      ; Hide everything but headings (all body lines)
;;     ("o" outline-hide-other)     ; Hide other branches
;;     ("c" outline-hide-entry)     ; Hide this entry's body
;;     ("l" outline-hide-leaves)    ; Hide body lines in this entry and sub-entries
;;     ("d" outline-hide-subtree)   ; Hide everything in this entry and sub-entries
;;     ;; Show
;;     ("a" outline-show-all)      ; Show (expand) everything
;;     ("e" outline-show-entry)    ; Show this heading's body
;;     ("i" outline-show-children) ; Show this heading's immediate child sub-headings
;;     ("k" outline-show-branches) ; Show all sub-headings under this heading
;;     ("s" outline-show-subtree) ; Show (expand) everything in this heading & below
;;     ;; Move
;;     ("u" outline-up-heading)               ; Up
;;     ("n" outline-next-visible-heading)     ; Next
;;     ("p" outline-previous-visible-heading) ; Previous
;;     ("f" outline-forward-same-level)       ; Forward - same level
;;     ("b" outline-backward-same-level)      ; Backward - same level
;;     ("q" nil "leave"))

;;   ;; (with-eval-after-load 'outline
;;   ;;   (bind-keys (:map outline-minor-mode-map
;;   ;;                    ("C-c #" . hydra-outline/body)
;;   ;;                    ;; Don't shadow smarparens or org bindings
;;   ;;                    ("M-<up>" . nil)
;;   ;;                    ("M-<down>" . nil)
;;   ;;                    ("<backtab>" . outshine-cycle-buffer)
;;   ;;                    ("M-=" . outline-show-current-sublevel)
;;   ;;                    ("M-p" . outline-subtree-previous)
;;   ;;                    ("M-n" . outline-subtree-next))))

;;   :hook
;;   (outline-minor-mode . outshine-mode)
;;   (prog-mode . outline-minor-mode))
  

(use-package outorg
  :defer 4
  :after outshine)

;; hs-minor-mode for folding top level forms
(use-package hideshow
  :defer 4
  :custom
  (hs-hide-comments-when-hiding-all nil)
  :config
  (defhydra hydra-hs (:color pink :hint nil)
    "
Hideshow

Hide^^            ^Show^            ^Toggle^    ^Navigation^
----------------------------------------------------------------
_h_ hide all      _s_ show all      _t_ toggle    _n_ next line
_d_ hide block    _a_ show block                _p_ previous line
_l_ hide level

_q_ quit
"
    ("s" hs-show-all)
    ("h" hs-hide-all)
    ("a" hs-show-block)
    ("d" hs-hide-block)
    ("t" hs-toggle-hiding)
    ("l" hs-hide-level)
    ("n" forward-line)
    ("p" (forward-line -1))
    ("q" nil))
  :bind
  (:map hs-minor-mode-map
        ("C-c @" . hydra-hs/body)
        ("C-<tab>" . hs-toggle-hiding))
  :hook
  (hs-minor-mode . hs-hide-all))

(use-package symbol-overlay
  :hook
  (prog-mode . symbol-overlay-mode)
  :bind
  ("C-s-n" . symbol-overlay-jump-next)
  ("C-s-p" . symbol-overlay-jump-prev)
  ("C-s-r" . symbol-overlay-rename)
  ("C-s-5" . symbol-overlay-query-replace))

(use-package iy-go-to-char
  :custom
  (iy-go-to-char-forward ?f)
  (iy-go-to-char-backward ?b)
  :config
  (with-eval-after-load 'multiple-cursors
    (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos))
  :bind
  (("M-F" . iy-go-up-to-char)
   ("M-B" . iy-go-up-to-char-backward)
   ("C-M-\"" . iy-go-to-or-up-to-continue)
   ("C-M-:" . iy-go-to-or-up-to-continue-backward)))

(use-package rainbow-mode
  :hook
  ((css-mode emacs-lisp-mode sass-mode) . rainbow-mode))

(use-package crux
  :bind
  ("C-c C-j" . crux-eval-and-replace)
  ("s-<backspace>" . crux-kill-line-backwards)
  ("C-M-X" . crux-indent-defun)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("C-c R" . crux-rename-file-and-buffer)
  ("C-c k" . crux-kill-other-buffers)
  ("C-c C-o" . crux-open-with)
  ("C-c S" . crux-find-shell-init-file))

(defun save-kill-buffers-and-quit ()
  "Kill all buffers, clean up tramp caches, and quit Emacs."
  (interactive)
  (save-some-buffers)
  (desktop-clear)
  (tramp-cleanup-all)
  (kill-emacs))

(defmacro specialize-beginning-of-buffer (mode &rest forms)
  "Define a special version of `beginning-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-min' and then FORMS are evaluated. If the point did not
change because of the evaluation of FORMS, jump unconditionally
to `point-min'. This way repeated invocations toggle between real
beginning and logical beginning of the buffer.

https://fuco1.github.io/2017-05-06-Enhanced-beginning--and-end-of-buffer-in-special-mode-buffers-(dired-etc.).html"
  (declare (indent 1))
  (let ((fname (intern (concat (symbol-name mode) "-beginning-of-buffer")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-min))
           ,@forms
           (when (= p (point))
             (goto-char (point-min)))))
       (add-hook ',mode-hook
                 (lambda ()
                   (define-key ,mode-map
                     [remap beginning-of-buffer] ',fname))))))

(defmacro specialize-end-of-buffer (mode &rest forms)
  "Define a special version of `end-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-max' and then FORMS are evaluated.  If the point did
not change because of the evaluation of FORMS, jump
unconditionally to `point-max'.  This way repeated invocations
toggle between real end and logical end of the buffer.

https://fuco1.github.io/2017-05-06-Enhanced-beginning--and-end-of-buffer-in-special-mode-buffers-(dired-etc.).html"
  (declare (indent 1))
  (let ((fname (intern (concat "my-" (symbol-name mode) "-end-of-buffer")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-max))
           ,@forms
           (when (= p (point))
             (goto-char (point-max)))))
       (add-hook ',mode-hook
                 (lambda ()
                   (define-key ,mode-map
                     [remap end-of-buffer] ',fname))))))

(specialize-beginning-of-buffer dired
  (while (not (ignore-errors (dired-get-filename))) (dired-next-line 1)))
(specialize-end-of-buffer dired (dired-previous-line 1))

(specialize-beginning-of-buffer occur (occur-next 1))
(specialize-end-of-buffer occur (occur-prev 1))

(specialize-beginning-of-buffer ivy-occur-grep (ivy-occur-next-line 1))
(specialize-end-of-buffer ivy-occur-grep (ivy-occur-previous-line 1))

(specialize-beginning-of-buffer ibuffer (ibuffer-forward-line 1))
(specialize-end-of-buffer ibuffer (ibuffer-backward-line 1))

(specialize-beginning-of-buffer vc-dir (vc-dir-next-line 1))
(specialize-end-of-buffer vc-dir (vc-dir-previous-line 1))

(specialize-beginning-of-buffer bs (bs-down 2))
(specialize-end-of-buffer bs (bs-up 1) (bs-down 1))

(specialize-beginning-of-buffer recentf-dialog
  (when (re-search-forward "^  \\[" nil t) (goto-char (match-beginning 0))))
(specialize-end-of-buffer recentf-dialog (re-search-backward "^  \\[" nil t))

(specialize-beginning-of-buffer org-agenda (org-agenda-next-item 1))
(specialize-end-of-buffer org-agenda (org-agenda-previous-item 1))

(specialize-beginning-of-buffer rg (compilation-next-error 1))
(specialize-end-of-buffer rg (compilation-previous-error 1))

(specialize-end-of-buffer elfeed-search (forward-line -2))

(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
  ^Mark^         ^Actions^         ^View^          ^Select^              ^Navigation^
  _m_ mark      _D_ delete       _g_ refresh    _q_ quit             _k_   ↑    _h_
  _u_ unmark    _s_ save marked  _S_ sort       _TAB_ toggle         _RET_ visit
  _*_ specific  _a_ all actions  _/_ filter     _o_ other window     _j_   ↓    _l_
  _t_ toggle    _._ toggle hydra _H_ help       C-o other win no-select
  "
  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)
  ("t" ibuffer-toggle-marks)

  ("D" ibuffer-do-delete)
  ("s" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("S" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)
  ("H" describe-mode :color blue)

  ("h" ibuffer-backward-filter-group)
  ("k" ibuffer-backward-line)
  ("l" ibuffer-forward-filter-group)
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)

  ("TAB" ibuffer-toggle-filter-group)

  ("o" ibuffer-visit-buffer-other-window :color blue)
  ("q" quit-window :color blue)
  ("." nil :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                                     :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                       :after-exit
                                       (if (eq major-mode 'ibuffer-mode)
                                           (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(with-eval-after-load 'ibuffer
  (bind-keys :map ibuffer-mode-map
             ("." . hydra-ibuffer-main/body)))

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
 ("M-s-q" . save-kill-buffers-and-quit)
 ("s-z" . undo)
 ("C-z" . undo)
 ("s-x" . kill-line-or-region)
 ("s-c" . copy-line-or-region)
 ("s-v" . clipboard-yank-and-indent)
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
 ("s-/" . comment-toggle)
 ("s-h" . ns-do-hide-emacs)
 ("s-H" . ns-do-hide-others)
 ("C-c U" . revert-buffer)
 ("C-c i" . os-reveal-file)
 ("s-<return>" . eval-last-sexp)
 ("s-RET" . eval-last-sexp)
 ("s-n" . scratch-new-buffer)
 ("s-N" . scratch-new-buffer-other-window)
 ("C-c C-n"f . scratch-new-buffer)
 ("C-c M-n" . scratch-new-buffer-other-window)
 ("C-S-p" . previous-line-margin)
 ("C-S-n" . next-line-margin)
 ;; Scroll the buffer while the point remains stationary relative to the window.
 ;; TODO: Retain column position.
 ("H-p" . "\C-u1\M-v")
 ("H-n" . "\C-u1\C-v")

 ;; Full screen
 ("C-s-f" . fullscreen)

 ;; Quick switch buffers
 ("C-x C-b" . ibuffer)
 ("s-}" . next-buffer)
 ("C-c }" . next-buffer)
 ("s-{" . previous-buffer)
 ("C-c {" . previous-buffer)
 ("C-s-j" . switch-to-buffer-by-mode)
 ("C-c M-j" . switch-to-buffer-by-mode)

 ;; windmove
 ("H-a" . windmove-left)
 ("H-d" . windmove-right)
 ("H-w" . windmove-up)
 ("H-s" . windmove-down)
 ("M-]" . windmove-right)
 ("M-[" . windmove-left)

 ;; Resize windows
 ("M-s-<up>" . shrink-window)
 ("M-s-<down>" . enlarge-window)
 ("M-s-<left>" . shrink-window-horizontally)
 ("M-s-<right>" . enlarge-window-horizontally)

 ;; Kill buffer and window at the same time.
 ("M-s-w" . kill-buffer-and-window)
 ("M-s-W" . kill-other-buffer-and-window)

 ;; Navigate with mark
 ("M-s-," . pop-to-mark-command)
 ("C-c ," . pop-to-mark-command)
 ("s-," . pop-global-mark)
 ("C-c C-," . pop-global-mark)

 ;; Tags
 ("s-R" . xref-find-definitions-other-window)
 ("C-c M-r" . xref-find-definitions-other-window)

 ("C-c C-f" . find-file-at-point-with-line)

 :map ctl-x-4-map
 ("t" . toggle-window-split))

(provide 'm-navigate)

;;; m-navigate.el ends here
