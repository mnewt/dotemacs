;;; Init.el --- Emacs init file --- -*- lexical-binding: t -*-

;;; Commentary:
;; It's an Emacs init file. Relies on heavily on use-package for its
;; organization and performance features.

;;; Code:

(defconst emacs-start-time (current-time))

(setq debug-on-error t)

(defvar elisp-directory "~/.emacs.d/lisp"
  "Local elisp configuration files go here.")

(defvar m-elisp-modules
  '(
    start
    package
    library
    bind
    environ
    persist
    private
    appear
    help
    note
    navigate
    search
    file
    net
    vc
    edit
    shell
    eshell
    lisp
    mode
    ))

(dolist-with-progress-reporter
    (s m-elisp-modules)
    (message "Loading modules...")
  (load-file (format "%s/m-%s.el" elisp-directory (symbol-name s))))

(provide 'init)

;;; init.el ends here
