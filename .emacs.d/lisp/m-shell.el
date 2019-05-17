;;; m-shell-common.el --- Shell Configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Shell, Term, Tramp, and related things.

;;; Code:

(require 'tramp)

(use-package sh-script
  :custom
  (sh-basic-offset tab-width)
  (sh-indentation tab-width)
  ;; Tell `executable-set-magic' to insert #!/usr/bin/env interpreter
  (executable-prefix-env t)
  :hook
  ;; Make a shell script executable automatically on save
  (after-save . executable-make-buffer-file-executable-if-script-p)
  (before-save . maybe-reset-major-mode)
  :bind
  (:map sh-mode-map
        ("s-<ret>" . eshell-send-current-line)))

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

(defun expand-environment-variable ()
  "Insert contents of an envionment variable at point."
  (interactive)
  (insert (getenv (read-envvar-name "Insert Environment Variable: "))))

(defun tramp-cleanup-all ()
  "Clean up all tramp buffers and connections."
  (interactive)
  (tramp-cleanup-all-buffers)
  (tramp-cleanup-all-connections))

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
           (remove-if
            (apply-partially #'string-match "^/sshx\?:\\([a-z]+\\):")
            recentf-list))))

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

(defun dired-tramp (host)
  "Choose an ssh HOST and then open it with dired."
  (interactive (list (ssh-choose-host "Hostname or tramp string: ")))
  (find-file
   (if (tramp-file-name-p host)
       host
     (find-file (concat "/ssh:" host ":")))))

;; http://whattheemacsd.com/setup-shell.el-01.html
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  "`C-d' on an empty line in the shell terminates the process, accepts ARG."
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(defun shell-command-exit-code (program &rest args)
  "Run PROGRAM with ARGS and return the exit code."
  (with-temp-buffer
    (apply 'call-process program nil (current-buffer) nil args)))

;; dtach (https://github.com/crigler/dtach)
;; https://emacs.stackexchange.com/questions/2283/attach-to-running-remote-shell-with-eshell-tramp-dtach
(defvar explicit-dtach-args
  '("-A" "/tmp/emacs.dtach" "-z" "bash" "--noediting" "--login")
  "Args for dtach.")

(defun ssh-dtach (host)
  "Open SSH connection to HOST and create or attach to dtach session."
  (interactive (list (ssh-choose-host "SSH using dtach to host: ")))
  (let ((explicit-shell-file-name "dtach")
        (default-directory (format  "/sshx:%s:" host))
        (explicit-dtach-args explicit-dtach-args))
    (shell (format "*ssh (dtach) %s*" host))))

(defun ssh (host)
  "Open SSH connection to HOST and create or attach to dtach session."
  (interactive (list (ssh-choose-host "SSH using dtach to host: ")))
  (shell (format "*ssh %s*" host)))

(require 'term)

;; https://www.emacswiki.org/emacs/ShellMode
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

;; (defun sudo-enable ()
;;   "Enable sudo in the current buffer."
;;   (interative)
;;   (let* ((position (point))
;;          (f (expand-file-name (or buffer-file-name default-directory)))
;;          (newf (sudo-toggle--add-sudo f))
;;          ;; So that you don't get method overrides.
;;          (tramp-default-proxies-alist nil))
;;     (cond ((or buffer-file-name (derived-mode-p 'dired-mode))
;;            (find-file newf)
;;            (goto-char position))
;;           ((derived-mode-p 'shell-mode)
;;            (let ((b (format "*shell/sudo:root@%s*"
;;                             (with-parsed-tramp-file-name newf nil host))))
;;              (get-buffer-create b
;;                                 (cd newf)
;;                                 (shell b))))
;;           ((derived-mode-p 'eshell-mode)
;;            (eshell-return-to-prompt)
;;            (insert (concat "cd '" newf "'"))
;;            (eshell-send-input))
;;           (t (message "Can't sudo this buffer.")))))

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
         ;; So that you don't get method overrides.
         (tramp-default-proxies-alist nil))
    (cond ((or buffer-file-name (derived-mode-p 'dired-mode))
           (find-file newf)
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

;; (defun filter-functions (regexp &optional predicate)
;;   "Return a list of functions matching REGEXP.

;; If PREDICATE is specified, only return functions for which
;; predicate returns true."
;;   (let (fs)
;;     (mapatoms (lambda (x)
;;                 (when (and (fboundp x) (string-match-p regexp (symbol-name x))
;;                            (or (not predicate) (funcall predicate x)))
;;                   (push x fs))))
;;     fs))

;; TODO: Finish `maybe-with-sudo'.
;; (defun maybe-with-sudo (f &rest args)
;;   "If the command is prefixed with sudo, use tramp to sudo.

;; This is because `shell-command' and friends can't ask for sudo
;; privileges on their own since they do not have any way to prompt
;; the user for input.

;; NOTE: Will barf if you supply switches to sudo."
;;   (let* ((i ())
;;          (command (stringp (car args)) (car args) (cadr args)))
;;     (if (and (not (string-match-p "sudo:" default-directory))
;;              (string-match "^sudo " command))
;;         (progn (setq command (substring command 5))
;;                (unwind-protect
;;                    (progn (sudo-toggle)
;;                           (apply f command args))
;;                  (sudo-toggle)))
;;       (apply f command args))))

;; (seq-doseq (c (filter-functions "shell-command"
;;                                 (lambda (f) (commandp (symbol-function f)))))
;;   (advice-add c :around #'maybe-with-sudo))


;; Load `vterm' if it's available.
(ignore-errors
  (let ((vterm-dir "~/code/emacs-libvterm"))
    (when (file-exists-p vterm-dir)
      (add-to-list 'load-path "~/code/emacs-libvterm")
      (let (vterm-install)
        (require 'vterm)))))

(use-package term
  :bind
  (("C-c t" . vterm)
   :map term-mode-map
   ("M-p" . term-send-up)
   ("M-n" . term-send-down)
   :map term-raw-map
   ("M-o" . other-window)
   ("M-p" . term-send-up)
   ("M-n" . term-send-down)
   ("C-M-j" . term-switch-to-shell-mode)))

;; Adapted from https://stackoverflow.com/a/42666026/1588358
(defun xterm-color-apply-on-minibuffer ()
  "Apply xterm color filtering on minibuffer output."
  (let ((bufs (cl-remove-if-not
               (lambda (x) (string-prefix-p " *Echo Area" (buffer-name x)))
               (buffer-list))))
    (dolist (buf bufs)
      (with-current-buffer buf
        (xterm-color-colorize-buffer)))))

(defun xterm-color-apply-on-minibuffer-advice (_proc &rest _rest)
  "Wrap `xterm-color-apply-on-minibuffer'."
  (xterm-color-apply-on-minibuffer))

(defun xterm-color-apply-on-shell-command-advice (_proc &rest _rest)
  "Apply xterm color filtering on shell command output."
  (with-current-buffer "*Shell Command Output*" (xterm-color-colorize-buffer)))

(use-package xterm-color
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (advice-add #'shell-command :after #'xterm-color-apply-on-minibuffer-advice)
  (advice-add #'shell-command-on-region :after #'xterm-color-apply-on-shell-command-advice)
  :commands
  (xterm-color-filter xterm-color-apply-on-minibuffer)
  :hook
  (shell-mode
   . (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
  (compilation-start
   . (lambda (proc)
       ;; We need to differentiate between compilation-mode buffers
       ;; and running as part of comint (which at this point we assume
       ;; has been configured separately for xterm-color)
       (when (eq (process-filter proc) 'compilation-filter)
         ;; This is a process associated with a compilation-mode buffer.
         ;; We may call `xterm-color-filter' before its own filter function.
         (set-process-filter proc
                             (lambda (proc string)
                               (funcall 'compilation-filter proc
                                        (xterm-color-filter string))))))))

(use-package bash-completion
  :custom
  ;; So that it doesn't sometimes insert a space ('\ ') after completing the
  ;; file name.
  (bash-completion-nospace t)
  :hook
  (shell-dynamic-complete-functions . bash-completion-dynamic-complete))

(use-package fish-mode
  :custom (fish-indent-offset tab-width)
  :mode "\\.fish\\'")

(use-package fish-completion
  :custom
  (fish-completion-fallback-on-bash-p t)
  :hook
  (after-init . global-fish-completion-mode))

(use-package company-shell
  :config
  (add-to-list
   'company-backends
   `(company-shell company-shell-env
                   ,(when (executable-find "fish") 'company-fish-shell))))

(bind-keys ("C-c C-v" . expand-environment-variable)
           ("C-:" . tramp-insert-remote-part)
           :map shell-mode-map
           ("C-d" . comint-delchar-or-eof-or-kill-buffer)
           ("SPC" . comint-magic-space))

(provide 'm-shell)

;;; m-shell-common.el ends here
