;;; environ.el --- Operating System and environment -*- lexical-binding: t -*-

;;; Commentary:

;; Set up Operating System and Environment details.

;;; Code:

(require 'cl-seq)

(defvar datetime-timezone)
(setq datetime-timezone 'US/Pacific)

;; Path
(defvar path-default (if (eq system-type 'windows-nt)
                             '("C:/bin" "C:/Program Files/Emacs/bin")
                           nil)
  "Defines a list of path entries to add by default.")

(defvar path-user nil
  "Defines a list of path entries to add to all systems.")

(defun source-sh (filename)
  "Sources FILENAME using the user's login shell.
Update environment variables from a shell source file."
  (interactive "fSource file: ")
  (message "Sourcing environment from `%s'..." filename)
  (with-temp-buffer
    (shell-command (format "diff -u <(true; export) <(source %s; export)" filename) '(4))
    (let ((envvar-re "declare -x \\([^=]+\\)=\\(.*\\)$"))
      ;; Remove environment variables
      (while (search-forward-regexp (concat "^-" envvar-re) nil t)
        (let ((var (match-string 1)))
          ;; (message "%s" (prin1-to-string `(setenv ,var nil)))
          (setenv var nil)))
      ;; Update environment variables
      (goto-char (point-min))
      (while (search-forward-regexp (concat "^+" envvar-re) nil t)
        (let ((var (match-string 1))
              (value (read (match-string 2))))
          ;; (message "%s" (prin1-to-string `(setenv ,var ,value)))
          (setenv var value)))))
  (message "Sourcing environment from `%s'... done." filename))

(defun path-add (&rest paths)
  "Add PATHS to the OS and Emacs executable search paths."
  (let* ((old-path (split-string (getenv "PATH") path-separator))
         new-path)
    (dolist (path (append paths old-path))
      (setq path (expand-file-name path))
      (when (file-directory-p path) (cl-pushnew path new-path :test #'string=)))
    (setenv "PATH" (mapconcat #'identity new-path path-separator))
    (setq exec-path new-path)))

(defun path-reset ()
  "Set path variables correctly for Linux, macOS, or Windows."
  (apply #'path-add (append path-user path-default)))

(source-sh "~/.env")
(source-sh "~/.bin/start-ssh-agent")
(path-reset)

(defvar os-open-file-executable nil
  "The executable used to open files in the host OS GUI.")

(defun config-unix ()
  "Configure Emacs for common Unix (Linux and macOS) settings."
  nil)

(defun config-linux ()
  "Configure Emacs for Linux."
  (config-unix))

(defun config-macos ()
  "Configure Emacs for macOS."
  (config-unix)
  (setq ns-alternate-modifier 'meta
        ns-right-alternate-modifier 'none
        ns-command-modifier 'super
        ns-right-command-modifier 'left
        ns-control-modifier 'control
        ns-right-control-modifier 'left
        ns-function-modifier 'hyper
        ;; Open files from Finder in same frame.
        ns-pop-up-frames nil
        os-open-file-executable "open")  
  ;; Use system trash
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash"))

(defvar w32-pass-lwindow-to-system)
(defvar w32-lwindow-modifier)
(defvar w32-pass-rwindow-to-system)
(defvar w32-rwindow-modifier)

(defun config-windows ()
  "Configure Emacs for Windows."
  (setq w32-pass-lwindow-to-system nil
        w32-lwindow-modifier 'super
        w32-pass-rwindow-to-system nil
        w32-rwindow-modifier 'super
        os-open-file-executable "explorer"))

;; OS specific configuration
(pcase system-type
  ('darwin (config-macos))
  ('gnu/linux (config-linux))
  ('windows-nt (config-windows))
  ('cygwin (config-windows)))

(provide 'm-environment)

;;; environ.el ends here
