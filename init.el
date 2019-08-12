;;; init.el --- Emacs init file --- -*- lexical-binding: t -*-

;;; Commentary:
;; It's an Emacs init file. Relies on heavily on use-package for its
;; organization and performance features.

;;; Code:

(defconst emacs-start-time (current-time))

;; (setq debug-on-error t)

(defconst init-files
  '(start
    env
    package
    lib
    bind
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
    mode)
  "List of init files to load.")

(dolist-with-progress-reporter
    (s init-files)
    (message "Loading modules...")
  (load (concat "~/.emacs.d/init/" (symbol-name s)) nil t))

(provide 'init)

;;; init.el ends here
