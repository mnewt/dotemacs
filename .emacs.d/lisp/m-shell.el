;;; m-shell-common.el --- Shell Configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Shell, Term, Tramp, Shell scripting, and related things.

;;; Code:

(use-package comint
  :ensure nil
  :custom
  (comint-buffer-maximum-size 20000)
  (comint-prompt-read-only t))

(use-package ssh
  :custom
  (ssh-directory-tracking-mode 'ftp)
  :commands
  ssh)

(defun tramp-cleanup-all ()
  "Clean up all tramp buffers and connections."
  (interactive)
  (cl-letf (((symbol-function #'password-reset)
             (lambda ())))
    (tramp-cleanup-all-connections))
  (setq ivy-history
        (seq-remove (lambda (s) (file-remote-p (substring-no-properties s)))
                    ivy-history)))

(defun tramp-insert-remote-part ()
  "Insert current tramp prefix at point."
  (interactive)
  (if-let* ((remote (file-remote-p default-directory)))
      (insert remote)))

;; Configure TRAMP to respect the PATH variable on the remote machine (for
;; remote eshell sessions)
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(defun expand-environment-variable ()
  "Insert contents of an envionment variable at point."
  (interactive)
  (insert (getenv (read-envvar-name "Insert Environment Variable: "))))

(defun list-hosts-from-known-hosts ()
  "Return a list of hosts from `~/.ssh/known_hosts'."
  (with-temp-buffer
    (insert-file-contents "~/.ssh/known_hosts")
    (cl-remove-if (lambda (host) (string= "" host))
                  (mapcar (lambda (line) (replace-regexp-in-string
                                          "\\]\\|\\[" ""
                                          (car (split-string line "[, :]"))))
                          (split-string (buffer-string) "\n")))))

(defun list-hosts-from-ssh-config ()
  "Return a list of hosts from `~/.ssh/config'."
  (with-temp-buffer
    (insert-file-contents "~/.ssh/config")
    (keep-lines "^Host")
    (cl-remove-if (lambda (host) (or (string=  "" host) (string= "*" host)))
                  (mapcar (lambda (line) (replace-regexp-in-string "Host +" "" line))
                          (split-string (buffer-string) "\n")))))

(defun list-hosts-from-etc-hosts ()
  "Return a list of hosts from `/etc/hosts'."
  (with-temp-buffer
    (insert-file-contents "/etc/hosts")
    (flush-lines "^#")
    (flush-lines "^$")
    (cl-remove-if (lambda (host) (or (string= host "localhost")
                                     (string= host "broadcasthost")
                                     (eq host nil)))
                  (mapcar (lambda (line) (cadr (split-string line "[ \t]+")))
                          (split-string (buffer-string) "\n")))))

(defun list-hosts-from-recentf ()
  "Return a list of hosts from the `recentf-list'."
  (cl-remove-duplicates
   (mapcar (lambda (s)
             (replace-regexp-in-string
              ":.*" ""
              (replace-regexp-in-string "^/sshx\?:" "" s)))
           (cl-remove-if
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
                     (list-hosts-from-etc-hosts)))))

(defun dired-tramp (host)
  "Choose an ssh HOST and then open it with dired."
  (interactive (list (ssh-choose-host "Hostname or tramp string: ")))
  (find-file
   (if (tramp-file-name-p host)
       host
     (find-file (concat "/ssh:" host ":")))))

(use-package shell
  :commands
  comint-delchar-or-maybe-eof
  :config
  ;; http://whattheemacsd.com/setup-shell.el-01.html
  (defun comint-delchar-or-eof-or-kill-buffer (arg)
    "`C-d' on an empty line in the shell terminates the process, accepts ARG."
    (interactive "p")
    (if (null (get-buffer-process (current-buffer)))
        (kill-buffer)
      (comint-delchar-or-maybe-eof arg)))

  (defun shell-rename-buffer (_)
    "Rename buffer to `default-directory'."
    (rename-buffer (format "*Shell: %s*" default-directory) t))
  :commands
  shell
  :hook
  (shell-mode-hook . shell-dirtrack-mode)
  (comint-output-filter-functions . shell-rename-buffer)
  :bind
  (:map shell-mode-map
        ("C-d" . comint-delchar-or-eof-or-kill-buffer)
        ("SPC" . comint-magic-space)
        ("M-r" . counsel-shell-history)))

(use-package bash-completion
  :custom
  ;; So that it doesn't sometimes insert a space ('\ ') after completing the
  ;; file name.
  (bash-completion-nospace t)
  :hook
  (shell-dynamic-complete-functions . bash-completion-dynamic-complete))

(use-package fish-completion
  :ensure-system-package fish
  :custom
  (fish-completion-fallback-on-bash-p t)
  :hook
  ((eshell-mode-hook shell-mode-hook) . fish-completion-mode))

(use-package company-shell
  :config
  (add-to-list
   'company-backends
   `(company-shell company-shell-env
                   ,(when (executable-find "fish") 'company-fish-shell))))
;; dtach (https://github.com/crigler/dtach)
;; https://emacs.stackexchange.com/questions/2283/attach-to-running-remote-shell-with-eshell-tramp-dtach
(defvar explicit-dtach-args
  '("-A" "/tmp/emacs.dtach" "-z" "bash" "--noediting" "--login")
  "Args for dtach.")

(defun ssh-dtach (host)
  "Open SSH connection to HOST and create or attach to dtach session."
  (interactive (list (ssh-choose-host "SSH using dtach to host: ")))
  (let ((explicit-shell-file-name "dtach")
        (default-directory (format  "/ssh:%s:" host)))
    (shell (format "*ssh (dtach) %s*" host))))

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

(defun sudo-toggle ()
  "Reopen the current file, directory, or shell as root.

For files and dired buffers, the non-sudo buffer is replaced with
a sudo buffer.

For shells, a sudo shell is opened but the
non-sudo shell is left intact."
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

(defun filter-functions (regexp &optional predicate)
  "Return a list of functions matching REGEXP.

If PREDICATE is specified, only return functions for which
predicate returns true."
  (let (fs)
    (mapatoms (lambda (x)
                (when (and (fboundp x) (string-match-p regexp (symbol-name x))
                           (or (not predicate) (funcall predicate x)))
                  (push x fs))))
    fs))

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

(use-package vterm
  :git "https://github.com/akermu/emacs-libvterm.git"
  :init
  (defun vterm--rename-buffer-as-title (title)
    (rename-buffer (format "*VTerm %s*" title) t))
  
  (defun vterm--set-background-color ()
    (make-local-variable 'ansi-color-names-vector)
    (aset ansi-color-names-vector 0
          (plist-get (face-spec-choose (fiat-theme-face 'default)) :background)))
  
  (defun maybe-enable-hl-line-mode ()
    (if (eq major-mode 'vterm-mode)
        (global-hl-line-mode -1)
      (global-hl-line-mode +1)))
  
  (defvar vterm-install t "Tell `vterm' to compile if necessary.")
  :config
  (add-to-list 'vterm-set-title-functions #'vterm--rename-buffer-as-title)
  :hook
  (vterm-mode-hook . vterm--set-background-color)
  (window-configuration-change-hook . maybe-enable-hl-line-mode)
  :bind
  ("s-t" . vterm)
  ("C-c t" . vterm)
  ("s-T" . vterm-other-window)
  ("C-c T" . vterm-other-window))

(use-package term
  :bind
  (:map term-mode-map
   ("M-p" . term-send-up)
   ("M-n" . term-send-down)
   :map term-raw-map
   ("M-o" . other-window)
   ("M-p" . term-send-up)
   ("M-n" . term-send-down)
   ("C-M-j" . term-switch-to-shell-mode)))

(use-package xterm-color
  :commands
  xterm-color-colorize-buffer
  xterm-color-filter
  :config
  (defun xterm-color-shell-command (&rest _rest)
    "Colorize the output of `shell-command'."
    (dolist (b (buffer-list))
      (pcase (buffer-name b)
        ("*Shell Command Output*"
         (with-current-buffer b (xterm-color-colorize-buffer)))
        (" *Echo Area 0*"
         (with-current-buffer b (xterm-color-colorize-buffer))))))

  (defun xterm-color-apply-on-compile (proc)
    "Apply xterm color filtering on the `compilation-mode' running PROC."
    ;; We need to differentiate between compilation-mode buffers
    ;; and running as part of comint (which at this point we assume
    ;; has been configured separately for xterm-color)
    (when (eq (process-filter proc) 'compilation-filter)
      ;; This is a process associated with a compilation-mode buffer.
      ;; We may call `xterm-color-filter' before its own filter function.
      (set-process-filter proc (lambda (proc string)
                                 (funcall 'compilation-filter proc
                                          (xterm-color-filter string))))))

  (defun xterm-color-shell-setup ()
    ;; Disable font-locking in this buffer to improve performance
    (font-lock-mode -1)
    ;; Prevent font-locking from being re-enabled in this buffer
    (make-local-variable 'font-lock-function)
    (setq font-lock-function (lambda (_) nil))
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))

  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (advice-add #'shell-command :after #'xterm-color-shell-command)
  (advice-add #'shell-command-on-region :after #'xterm-color-shell-command)

  (with-eval-after-load 'compile
    (eval-when-compile (defvar compilation-environment))
    (add-to-list 'compilation-environment "TERM=xterm-256color"))
  :hook
  (shell-mode-hook . xterm-color-shell-setup)
  (compilation-start-hook . xterm-color-apply-on-compile))

(defun fpw (command)
  "Run `fpw' command as COMMAND.

Copy the result to the `kill-ring'. Call with a prefix argument
to modify the args."
  (interactive (list (if current-prefix-arg
                         (read-shell-command "Run fpw (like this): "
                                             "fpw " 'pw-history)
                       "fpw")))
  (let ((result
         (replace-regexp-in-string "\n" "" (shell-command-to-string command))))
    (kill-new result)
    (message result)))

(bind-keys ("C-c C-v" . expand-environment-variable)
           ("C-:" . tramp-insert-remote-part)
           ("M-m p" . pw))

(provide 'm-shell)

;;; m-shell.el ends here
