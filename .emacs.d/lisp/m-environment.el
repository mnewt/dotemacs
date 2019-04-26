;;; m-environment.el --- Operating System and environment -*- lexical-binding: t -*-

;; Author: Matthew Newton


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

;; Path
(defvar set-path-unix nil
  "Defines a list of path entries to add to *NIX systems.")

(defvar set-path-windows
  '("C:/bin"
    "C:/Program Files/Emacs/bin")
  "Defines a list of path entries to add to Windows systems.")

(defvar set-path-user nil
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

(defun set-path ()
  "Set path variables correctly for Linux, macOS, or Windows."
  (let* ((os-specific-paths (if (eq system-type 'windows-nt)
                                set-path-windows
                              set-path-unix))
         (sep (if (eq system-type 'windows-nt) ";" ":"))
         (old-path (split-string (getenv "PATH") sep))
         ;; De-dupe and validate new path
         (new-path
          (-map 'expand-file-name
                (-filter 'file-directory-p
                         (-distinct (append set-path-user
                                            os-specific-paths
                                            old-path))))))
    (setenv "PATH" (apply 'concat (-interpose sep new-path)))
    ;; (message "New path: %s" new-path)
    (setq exec-path new-path)))

(source-sh "~/.env")
(source-sh "~/.bin/start-ssh-agent")
(set-path)

(add-hook 'after-init-hook (lambda ())
          (require 'server)
          (unless (server-running-p) (server-start)))

(use-package restart-emacs
  :commands
  (restart-emacs))

(defun config-unix ()
  "Configure Emacs for common Unix (Linux and macOS) settings."
  nil)

(defun config-linux ()
  "Configure Emacs for Linux."
  (config-unix)
  (set-face-font 'default "DejaVu Sans-12"))

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
  (when window-system (menu-bar-mode +1))
  (set-face-font 'default "Monaco-13")
  ;; Use system trash
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash"))

(declare w32-pass-lwindow-to-system)
(declare w32-lwindow-modifier)
(declare w32-pass-rwindow-to-system)
(declare w32-rwindow-modifier)

(defun config-windows ()
  "Configure Emacs for Windows."
  (menu-bar-mode -1)
  (setq w32-pass-lwindow-to-system nil
        w32-lwindow-modifier 'super
        w32-pass-rwindow-to-system nil
        w32-rwindow-modifier 'super
        os-open-file-executable "explorer")
  (set-face-font 'default "Lucida Console-12"))

;; OS specific configuration
(pcase system-type
  ('darwin (config-macos))
  ('gnu/linux (config-linux))
  ('windows-nt (config-windows))
  ('cygwin (config-windows)))

(provide 'm-environment)

;;; m-environment.el ends here
