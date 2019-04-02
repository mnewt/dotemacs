;;; m-eshell.el --- Eshell functionality and configuration -*- lexical-binding: t -*-

;; Author: Matthew Newton
;; Maintainer: Matthew Newton
;; Version: 0.1
;; Package-Requires: (eshell cl-extra)
;; Homepage: https://gitlab.com/mnewt/dotemacs
;; Keywords: eshell


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

;; This contains all my customziations of Eshell.

;;; Code:

;; ElDoc and topical help in Eshell.
(use-package esh-help
  :config
  (setup-esh-help-eldoc))

;; Fish-like autosuggestions.
(use-package esh-autosuggest
  :hook
  (eshell-mode . esh-autosuggest-mode)
  :bind
  (:map esh-autosuggest-active-map
        ("C-e" . company-complete-selection)))

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

(defun eshell-path-advice (f &rest paths)
  "For each element in F PATHS, return path relative to the host.
If the element starts with `:' and we are on a remote host).

Examples: > cd /etc -> /etc > cd :/etc -> /sshx:host:/etc > cd :
-> /sshx:host:/home/user"
  (apply f
         (cl-loop for path in (-flatten paths) collect
                  (if-let* ((remote (and (string-prefix-p ":" path)
                                         (file-remote-p path))))
                      (concat remote (substring path 1))
                    path))))

;; Advise functions to work with ":" path syntax (see `eshell-path-advice').
(seq-do (lambda (f) (advice-add f :around #'eshell-path-advice))
        '(eshell/cd eshell/cp eshell/mv eshell/rm eshell/e eshell/ee eshell/d
                    eshell/do))

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

(defun eshell/init ()
  "Initialize the Eshell environment."
  (source-sh "~/.env")
  (setq eshell-path-env (getenv "PATH"))
  ;; Path to shell executable. Set it this way to work with tramp.
  (setenv "ESHELL" "/bin/bash")
  ;; (setenv "TERM" "eterm-color")
  (setenv "EDITOR" "emacsclient")
  (setenv "PAGER" "cat")
  (setenv "MANPAGER" "cat")

  (bind-keys
   ("M-P" . eshell-send-previous-input)
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

  ;; Commands that use curses get launched in their own `term' buffer
  (seq-do (-partial #'add-to-list 'eshell-visual-commands)
          '("htop" "mbsync" "ncdu" "nnn" "nvim" "ssh" "tail" "tmux"
            "top" "vim" "w3m"))
  (seq-do (-partial #'add-to-list 'eshell-visual-subcommands)
          '(("git" "log" "diff" "show")
            ("dw" "log" "runshell" "shell")))

  ;; Load the Eshell versions of `su' and `sudo'
  (require 'em-tramp)
  (add-to-list 'eshell-modules-list 'eshell-tramp))

(defun ibuffer-show-eshell-buffers ()
  "Open an ibuffer window and display all Eshell buffers."
  (interactive)
  (ibuffer nil "Eshell Buffers" '((mode . eshell-mode)) nil t nil
           '(((name 64 64 :left)
              " "
              (process 0 -1 :right)))))

(defun eshell-create-in-background ()
  "Create a new Eshell buffer but don't display it."
  (let ((eshell-buffer-name (generate-new-buffer "*Eshell*")))
    (save-window-excursion (eshell))))

(defun eshell-get-or-create ()
  "Get or create an Eshell buffer."
  (interactive)
  (or (when current-prefix-arg (eshell-create-in-background))
      (some-buffer "*Eshell")
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

(defun eshell-ls-find-file-at-point (point)
  "RET on Eshell's `ls' output to open files."
  (interactive "d")
  (find-file (substring-no-properties (thing-at-point 'filename))))

(defvar m-eshell-ls-file-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'eshell-ls-find-file-at-point)
    (define-key map (kbd "<return>") 'eshell-ls-find-file-at-point)
    (define-key map [mouse-1] 'eshell-ls-find-file-at-point)
    map)
  "Keys in effect when point is over a file in `eshell/ls'
  output.")

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

(advice-add 'eshell-ls-decorated-name :around #'m-eshell-ls-decorated-name)

(provide 'm-eshell)

;;; m-eshell.el ends here
