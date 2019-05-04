;;; m-eshell.el --- Eshell functionality and configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Customize Eshell.

;;; Code:

;; ElDoc and topical help in Eshell.
(use-package esh-help
  :config
  (setup-esh-help-eldoc)
  :commands
  (setup-esh-help-eldoc esh-help-run-help))

;; Fish-like autosuggestions.
(use-package esh-autosuggest
  :hook
  (eshell-mode . esh-autosuggest-mode)
  (esh-autosuggest-mode . (lambda () (bind-key "C-e"
                                               #'company-complete-selection
                                               esh-autosuggest-active-map))))
(defun eshell-other-window (arg)
  "Opens an eshell in another window, creating a new one if ARG is specified."
  (interactive "p")
  (if (= arg 4)
      (let* ((parent (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))
             ;; `eshell' uses this variable as the new buffer name
             (eshell-buffer-name (concat "*eshell: " (car (last (split-string parent "/" t))) "*")))
        (switch-to-buffer-other-window "*eshell-here-temp*")
        (eshell)
        (kill-buffer "*eshell-here-temp*")
        (insert (concat "ls"))
        (eshell-queue-input))
    (progn
      (switch-to-buffer-other-window "*eshell*")
      (eshell)
      (message (number-to-string arg)))))

(defun eshell-maybe-bol ()
  "Smarter `beginning-of-line' for Eshell."
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(defun eshell-quit-or-delete-char (arg)
  "Quit Eshell if `C-d' is specified, passing ARG on."
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp nil))
      (progn
        (eshell-life-is-too-much)
        (ignore-errors
          (delete-window)))
    (delete-char  arg)))

(declare-function eshell-previous-input 'eshell)

(defun eshell-send-previous-input (&optional arg)
  "Re-run the previous command with ARG in the last used eshell buffer."
  (interactive "*p")
  (with-current-buffer
      (cl-some (lambda (b) (eq 'eshell-mode (with-current-buffer b major-mode)))
               (buffer-list))
    (with-selected-window (get-buffer-window)
      (goto-char (point-max))
      (recenter 4))
    (eshell-previous-input arg)
    (eshell-send-input)))

(defun eshell-send-current-line ()
  "Insert text of current line in eshell and execute."
  (interactive)
  (require 'eshell)
  (let ((command (buffer-substring
                  (save-excursion
                    (beginning-of-line)
                    (point))
                  (save-excursion
                    (end-of-line)
                    (point)))))
    (let ((buf (current-buffer)))
      (unless (get-buffer eshell-buffer-name)
        (eshell))
      (display-buffer eshell-buffer-name t)
      (switch-to-buffer-other-window eshell-buffer-name)
      (goto-char (point-max))
      (eshell-kill-input)
      (insert command)
      (eshell-send-input)
      (goto-char (point-max))
      (switch-to-buffer-other-window buf))))

(declare-function eshell/pwd 'eshell)

(defun m-eshell-prompt-function ()
  "Produce a highlighted prompt for Eshell."
  (mapconcat
   (lambda (list)
     (when list
       (propertize (concat " " (car list) " ")
                   'read-only t
                   'font-lock-face (cdr list)
                   'front-sticky '(font-lock-face read-only)
                   'rear-nonsticky '(font-lock-face read-only))))
   `(,(unless (eshell-exit-success-p)
        `(,(number-to-string eshell-last-command-status)
          :background "red" :foreground "white" :weight bold))
     (,(abbreviate-file-name (eshell/pwd)) :background "cyan" :foreground "black")
     (,(if (zerop (user-uid)) "\n(#)" "\n()")
      :foreground ,(if (equal 'light (frame-parameter nil 'background-mode))
                       "black"
                     "white")
      :weight bold))
   ""))

(declare-function eshell/cd 'eshell)

(defun eshell/s (host)
  "Change directory to HOST via tramp."
  (eshell/cd (concat "/ssh:" host ":")))

(defun eshell/e (&optional path)
  "Eshell alias for `find-file', passing optional PATH."
  (find-file path))

(defun eshell/ee (&optional path)
  "Eshell alias for `find-file-other-window', passing optional PATH."
  (find-file-other-window path))

(defun eshell/d (&optional path)
  "Eshell alias for `dired', passing optional PATH."
  (dired path))

(defun eshell/do (&optional path)
  "Eshell alias for `dired-other-window', passing optional PATH."
  (dired-other-window path))

(defun tramp-colon-prefix-expand (path)
  "Expand a colon prefix with the remote prefix

Examples:
  > cd /etc -> /etc
  > cd :/etc -> /sshx:host:/etc
  > cd : -> /sshx:host:/home/user"
  (if (file-remote-p default-directory)
      (cond
       ((string-prefix-p ":" path)
        (concat (file-remote-p default-directory)
                (substring path 1)))
       (t path))
    path))

(defun tramp-colon-prefix-maybe-expand ()
  "If we just inserted a colon prefix, run `tramp-colon-prefix-expand' on it."
  (when (looking-back "\\_<:")
    (delete-backward-char 1)
    (insert (tramp-colon-prefix-expand (concat ":" (thing-at-point 'filename))))))

(defun tramp-colon-prefix-setup ()
  (make-local-variable 'post-self-insert-hook)
  (add-hook 'post-self-insert-hook #'tramp-colon-prefix-maybe-expand))

;; Advise `eshell/*' functions to work with ":" prefix path syntax.
(seq-doseq (c '(cd cp mv rm e ee d do))
  (advice-add (intern (concat "eshell/" (symbol-name c)))
              :filter-args
              (lambda (args) (mapcar #'tramp-colon-prefix-expand args))))

(defun eshell/really-clear ()
  "Call `eshell/clear' with an argument to really clear the buffer.
Call it a second time to print the prompt."
  (interactive)
  (eshell/clear 1)
  (eshell/clear))

(defun eshell/info (&rest args)
  "Invoke `info', optionally opening the Info system to car ARGS."
  (Info-directory)
  (let ((subject (car args)))
    (if (not (null subject))
        (let ((node-exists (ignore-errors (Info-menu subject))))
          (if (not node-exists)
              (format "No menu item `%s' in node `(dir)Top'." subject))))))

(defun eshell-create-send (cmd &optional name)
  "Create an eshell buffer named NAME and run CMD in it."
  (let ((eshell-buffer-name (or name cmd)))
    (eshell))
  (insert cmd)
  (eshell-queue-input))

(defun eshell-vertical-create-send (cmd &optional name)
  "Create an eshell buffer named NAME and run CMD in it.
Split the window vertically."
  (split-window-vertically)
  (eshell-create-send cmd name))

(defun eshell-kill-previous-output ()
  "Kill the output of the previous command."
  (interactive)
  (let ((inhibit-read-only t)
        (lines (count-lines (eshell-beginning-of-output)
                            (eshell-end-of-output))))
    ;; Kill region
    (kill-region (eshell-beginning-of-output) (eshell-end-of-output))
    (save-excursion
      ;; Write something in place of the text so we know what happened.
      (goto-char (eshell-beginning-of-output))
      (insert (format "--- Killed %d lines ---\n" lines)))))

(defun eshell-kill-previous-output-to-buffer ()
  "Move output of the previous command to a new buffer."
  (interactive)
  (eshell-kill-previous-output)
  (switch-to-buffer-other-window "*eshell-stdout*")
  (yank))

(defun eshell-copy-previous-output ()
  "Copies the output of the previous command to the kill ring."
  (interactive)
  (let ((lines (count-lines (eshell-beginning-of-output)
                            (eshell-end-of-output))))
    ;; Copy region to kill ring
    (copy-region-as-kill (eshell-beginning-of-output) (eshell-end-of-output))
    (message "Copied %d lines" lines)))

(defun local-set-minor-mode-key (mode key def)
  "Override MODE KEY binding with DEF.
Override a minor mode keybinding for the local buffer. Create or
alter keymaps, storing them in buffer-local variable
`minor-mode-overriding-map-alist' so that we get the bindings we
want and they are now shadowed.'. See
https://stackoverflow.com/a/14769115/1588358"
  (let* ((oldmap (cdr (assoc mode minor-mode-map-alist)))
         (newmap (or (cdr (assoc mode minor-mode-overriding-map-alist))
                     (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map oldmap)
                       (push `(,mode . ,map) minor-mode-overriding-map-alist)
                       map))))
    (define-key newmap key def)))

(defun eshell/import-aliases ()
  "Retrieve bash aliases and format them for import into Eshell."
  (shell-command ". ~/.env && . ~/.aliases && alias | sed -E \"s/^alias ([^=]+)='(.*)'$/alias \\1 \\2 \\$*/g; s/'\\\\''/'/g;\""
                 "*bash aliases*"))

(declare-function eshell-previous-prompt 'eshell)
(declare-function eshell-next-prompt 'eshell)

(defun eshell/init ()
  "Initialize the Eshell environment."

  (source-sh "~/.env")
  (setq eshell-path-env (getenv "PATH"))
  ;; Path to shell executable. Set it this way to work with tramp.
  (setenv "SHELL" "/bin/bash")
  ;; (setenv "TERM" "eterm-color")
  (setenv "EDITOR" "emacsclient")
  (setenv "PAGER" "cat")
  (setenv "MANPAGER" "cat")

  (bind-keys
   :map eshell-mode-map
   ("C-a" . eshell-maybe-bol)
   ("C-d" . eshell-quit-or-delete-char)
   ("<tab>" . completion-at-point)
   ("M-r" . counsel-esh-history)
   ("C-L" . eshell/really-clear)
   ("C-w" . eshell-kill-previous-output)
   ("C-M-w" . eshell-kill-previous-output-to-buffer)
   ("M-w" . eshell-copy-previous-output)
   ("s-v" . clipboard-yank)
   ("C-S-<backspace>" . eshell-kill-input)
   ("C-M-S-p" . eshell-previous-prompt)
   ("M-<up>" . eshell-previous-prompt)
   ("C-M-S-n" . eshell-next-prompt)
   ("M-<down>" . eshell-next-prompt)
   ("C-h C-e" . esh-help-run-help))

  ;; xterm colors
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

  ;; Load the Eshell versions of `su' and `sudo'
  (require 'em-tramp)
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (remove-hook 'eshell-before-prompt-hook 'eshell/init))

(defun eshell-ls-find-file-at-point ()
  "RET on Eshell's `ls' output to open files."
  (interactive)
  (find-file (substring-no-properties (thing-at-point 'filename))))

(defvar m-eshell-ls-file-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'eshell-ls-find-file-at-point)
    (define-key map (kbd "<return>") 'eshell-ls-find-file-at-point)
    (define-key map [mouse-1] 'eshell-ls-find-file-at-point)
    map)
  "Keys in effect when point is over a file in `eshell/ls'
  output.")

(declare-function eshell-ls-applicable 'eshell)

(defun m-eshell-ls-decorated-name (f &rest args)
  "Add more decoration to files in `eshell/ls' output.

* Mark directories with a `/'
* Mark execurables with a `*'
* Open files and directories with `RET' or `mouse-1'

Advise `eshell-ls-decorated-name'."
  (let* ((file (car args))
         (name (apply f args))
         (suffix
          (cond
           ;; Directory
           ((eq (cadr file) t)
            "/")
           ;; Executable
           ((and (/= (user-uid) 0)            ; root can execute anything
                 (eshell-ls-applicable (cdr file) 3 'file-executable-p (car file)))
            "*"))))
    (propertize
     (if (and suffix (not (string-suffix-p suffix name)))
         (concat name suffix)
       name)
     'keymap m-eshell-ls-file-keymap
     'mouse-face 'highlight)))

(defun ibuffer-show-eshell-buffers ()
  "Open an ibuffer window and display all Eshell buffers."
  (interactive)
  (ibuffer nil "Eshell Buffers" '((mode . eshell-mode)) nil t nil
           '(((name 64 64 :left) " " (process 0 -1 :right)))))

(defun eshell-create-in-background ()
  "Create a new Eshell buffer but don't display it."
  (let ((eshell-buffer-name (generate-new-buffer "*Eshell*")))
    (save-window-excursion (eshell))))

(defun eshell-get-or-create ()
  "Get or create an Eshell buffer."
  (interactive)
  (or (when current-prefix-arg (eshell-create-in-background))
      (car (filter-buffers-by-mode 'eshell-mode))
      (eshell-create-in-background)))

(defun eshell-switch-to-buffer ()
  "Switch to the most recent Eshell buffer or create a new one.)))
This is different than the normal `eshell' command in my setup
because I dynamically rename the buffer according to
`default-directory'."
  (interactive)
  (switch-to-buffer (eshell-get-or-create)))

(defun eshell-switch-to-buffer-other-window ()
  "Get or create an Eshell buffer, then switch to it."
  (interactive)
  (switch-to-buffer-other-window (eshell-get-or-create)))

(defun switch-to-eshell-buffer ()
  "Interactively choose an Eshell buffer."
  (interactive)
  (switch-to-buffer-by-mode 'eshell-mode))

(defun eshell-prompt-housekeeping ()
  (setq xterm-color-preserve-properties t)
  (rename-buffer (format "*%s*" default-directory) t))

(use-package eshell
  :custom
  (eshell-banner-message "")
  (eshell-buffer-shorthand t)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-error-if-no-glob t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  (eshell-prompt-function 'm-eshell-prompt-function)
  (eshell-prompt-regexp "^(#?) ")
  (eshell-highlight-prompt nil)
  (eshell-ls-clutter-regexp (regexp-opt '(".cache" ".DS_Store" ".Trash" ".lock"
                                          "_history" "-history" ".tmp" "~"
                                          "desktop.ini" "Icon\r" "Thumbs.db"
                                          "$RECYCLE_BIN" "lost+found")))
  :config
  (add-to-list 'eshell-visual-commands "n")
  (advice-add 'eshell-ls-decorated-name :around #'m-eshell-ls-decorated-name)
  :hook
  ((eshell-mode . eshell/init)
   (eshell-mode . tramp-colon-prefix-setup)
   (eshell-before-prompt . eshell-prompt-housekeeping))
  :bind
  (("s-e" . eshell-switch-to-buffer)
   ("C-c e" . eshell-switch-to-buffer)
   ("s-E" . eshell-switch-to-buffer-other-window)
   ("C-c E" . eshell-switch-to-buffer-other-window)
   ("C-s-e" . switch-to-eshell-buffer)
   ("M-E" . ibuffer-show-eshell-buffers)
   ("C-c M-e" . ibuffer-show-eshell-buffers)
   :map prog-mode-map
   ("M-P" . eshell-send-previous-input)))

(provide 'm-eshell)

;;; m-eshell.el ends here
