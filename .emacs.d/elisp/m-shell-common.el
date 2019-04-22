;;; m-shell-common.el --- Shell, Terminal, SSH, and Tramp common config -*- lexical-binding: t -*-

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

(require 'tramp)

;;;###autoload
(defun maybe-reset-major-mode ()
  "Reset the buffer's `major-mode' if a different mode seems like a better fit.
Mostly useful as a `before-save-hook', to guess mode when saving a
new file for the first time.
https://github.com/NateEag/.emacs.d/blob/9d4a2ec9b5c22fca3c80783a24323388fe1d1647/init.el#L137"
  (when (and
         ;; The buffer's visited file does not exist.
         (eq (file-exists-p (buffer-file-name)) nil)
         (eq major-mode 'fundamental-mode))
    (normal-mode)))

;;;###autoload
(defun expand-environment-variable ()
  "Insert contents of an envionment variable at point."
  (interactive)
  (insert (getenv (read-envvar-name "Insert Environment Variable: "))))

;;;###autoload
(defun tramp-cleanup-all ()
  "Clean up all tramp buffers and connections."
  (interactive)
  (tramp-cleanup-all-buffers)
  (tramp-cleanup-all-connections))

;;;###autoload
(defun tramp-insert-remote-part ()
  "Insert current tramp prefix at point."
  (interactive)
  (if-let* ((remote (file-remote-p default-directory)))
      (insert remote)))

;; Configure TRAMP to respect the PATH variable on the remote machine (for
;; remote eshell sessions)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(defun list-hosts-from-known-hosts ()
  "Return a list of hosts from `~/.ssh/known_hosts'."
  (with-temp-buffer
    (insert-file-contents "~/.ssh/known_hosts")
    (-remove (lambda (host) (string=  "" host))
             (mapcar (lambda (line)
                       (replace-regexp-in-string "\\]\\|\\[" ""
                                                 (car (split-string line "[, :]"))))
                     (split-string (buffer-string) "\n")))))

(defun list-hosts-from-ssh-config ()
  "Return a list of hosts from `~/.ssh/config'."
  (with-temp-buffer
    (insert-file-contents "~/.ssh/config")
    (keep-lines "^Host")
    (-remove (lambda (host) (or (string=  "" host) (string= "*" host)))
             (mapcar (lambda (line)
                       (replace-regexp-in-string "Host +" "" line))
                     (split-string (buffer-string) "\n")))))

(defun list-hosts-from-etc-hosts ()
  "Return a list of hosts from `/etc/hosts'."
  (with-temp-buffer
    (insert-file-contents "/etc/hosts")
    (flush-lines "^#")
    (flush-lines "^$")
    (-remove (lambda (host) (or (string= host "localhost")
                                (string= host "broadcasthost")
                                (eq host nil)))
             (mapcar (lambda (line) (cadr (split-string line "[ \t]+")))
                     (split-string (buffer-string) "\n")))))

(defun list-hosts-from-recentf ()
  "Return a list of hosts from the `recentf-list'."
  (-distinct
   (mapcar (lambda (s)
             (replace-regexp-in-string
              ":.*" ""
              (replace-regexp-in-string "^/sshx\?:" "" s)))
           (-filter
            (apply-partially #'string-match "^/sshx\?:\\([a-z]+\\):")
            recentf-list))))

;;;###autoload
(defun ssh-choose-host (&optional prompt)
  "Make a list of recent ssh hosts and interactively choose one with optional PROMPT."
  (completing-read (or prompt "SSH to Host: ")
                   (-distinct
                    (append
                     (list-hosts-from-recentf)
                     (list-hosts-from-known-hosts)
                     (list-hosts-from-ssh-config)
                     (list-hosts-from-etc-hosts)))
                   nil t))

;;;###autoload
(defun tramp-dired (host)
  "Choose an ssh HOST and then open it with dired."
  (interactive (list (ssh-choose-host "Hostname or tramp string: ")))
  (find-file
   (if (tramp-file-name-p host)
       host
     (find-file (concat "/ssh:" host ":")))))

;;;###autoload
(defun tramp-dired-sudo (host)
  "SSH to HOST, sudo to root, open dired."
  (interactive (list (ssh-choose-host "Hostname or tramp string: ")))
  (find-file
   (if (tramp-file-name-p host)
       host
     (concat "/ssh:" host "|sudo:root@" host ":"))))

;; http://whattheemacsd.com/setup-shell.el-01.html
;;;###autoload
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  "`C-d' on an empty line in the shell terminates the process, accepts ARG."
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

;;;###autoload
(defun shell-command-exit-code (program &rest args)
  "Run PROGRAM with ARGS and return the exit code."
  (with-temp-buffer
    (apply 'call-process program nil (current-buffer) nil args)))

;; dtach (https://github.com/crigler/dtach)
;; https://emacs.stackexchange.com/questions/2283/attach-to-running-remote-shell-with-eshell-tramp-dtach
(defvar explicit-dtach-args
  '("-A" "/tmp/emacs.dtach" "-z" "bash" "--noediting" "--login")
  "Args for dtach.")

;;;###autoload
(defun ssh-dtach (host)
  "Open SSH connection to HOST and create or attach to dtach session."
  (interactive (list (ssh-choose-host "SSH using dtach to host: ")))
  (let ((explicit-shell-file-name "dtach")
        (default-directory (format  "/sshx:%s:" host))
        (explicit-dtach-args explicit-dtach-args))
    (shell (format "*ssh (dtach) %s*" host))))

(require 'term)

;; https://www.emacswiki.org/emacs/ShellMode
;;;###autoload
(defun term-switch-to-shell-mode ()
  "Switch a term session to shell."
  (interactive)
  (if (or (equal major-mode 'term-mode))
      (progn
        (shell-mode)
        (set-process-filter  (get-buffer-process (current-buffer)) 'comint-output-filter)
        (local-set-key (kbd "C-M-j") 'term-switch-to-shell-mode)
        (compilation-shell-minor-mode 1)
        (comint-send-input))
    (progn
      (compilation-shell-minor-mode -1)
      (font-lock-mode -1)
      (set-process-filter  (get-buffer-process (current-buffer)) 'term-emulate-terminal)
      (term-mode)
      (term-char-mode)
      (term-send-raw-string (kbd "C-l")))))

;; Apply colors to `shell-command' minibuffer output.
;; Adapted from https://stackoverflow.com/a/42666026/1588358
(defun xterm-color-apply-on-minibuffer ()
  "Apply xterm color filtering on minibuffer output."
  (let ((bufs (cl-remove-if-not
               (lambda (x) (string-prefix-p " *Echo Area" (buffer-name x)))
               (buffer-list))))
    (dolist (buf bufs)
      (with-current-buffer buf
        (xterm-color-colorize-buffer)))))

;;;###autoload
(defun xterm-color-apply-on-minibuffer-advice (_proc &rest _rest)
  "Wrap `xterm-color-apply-on-minibuffer'."
  (xterm-color-apply-on-minibuffer))

(defun sudo-toggle--add-sudo (path)
  "Add sudo to file PATH string."
  (if (file-remote-p path)
      (with-parsed-tramp-file-name (expand-file-name path) nil
        (concat "/" method ":"
                (when user (concat user "@"))
                host "|sudo:root@" host ":" localname))
    (concat "/sudo:root@localhost:" (expand-file-name path))))

(defun sudo-toggle--remove-sudo (path)
  "Remove sudo from file PATH string."
  (cond
   ((string-match-p "/sudo:root@localhost:" path)
    (replace-regexp-in-string (getenv "HOME") "~" (substring path 21)))

   ((string-match-p "|sudo:root@" path)
    (replace-regexp-in-string "|sudo:root@[^:]*" "" path))))

(declare-function eshell-return-to-prompt 'eshell)

;;;###autoload
(defun sudo-toggle ()
  "Reopen the current file, directory, or shell as root.  For))))
files and dired buffers, the non-sudo buffer is replaced with a
sudo buffer.  For shells, a sudo shell is opened but the non-sudo
shell is left intact."
  (interactive)
  (let* ((position (point))
         (f (expand-file-name (or buffer-file-name default-directory)))
         (newf (if (string-match-p "sudo:" f)
                   (sudo-toggle--remove-sudo f)
                 (sudo-toggle--add-sudo f)))
         ;; so that you don't get method overrides
         (tramp-default-proxies-alist nil))
    (cond ((or buffer-file-name (derived-mode-p 'dired-mode))
           (find-alternate-file newf)
           (goto-char position))
          ((derived-mode-p 'shell-mode)
           (if (string-match-p "*shell/sudo:root@" (buffer-name))
               (kill-buffer-and-window)
             (with-temp-buffer
               (cd newf)
               (shell (format "*shell/sudo:root@%s*"
                              (with-parsed-tramp-file-name newf nil host))))))
          ((derived-mode-p 'eshell-mode)
           (eshell-return-to-prompt)
           (insert (concat "cd '" newf "'"))
           (eshell-send-input))
          (t (message "Can't sudo this buffer")))))

(provide 'm-shell-common)

;;; m-shell-common.el ends here
