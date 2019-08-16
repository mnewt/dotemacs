;;; lib.el --- Emacs Lisp Libraries -*- lexical-binding: t -*-

;;; Commentary:

;; Common libraries and associated functions.

;;; Code:

;; Get rid of prompting for disabled commands.
(setq disabled-command-function nil)

(require 'seq)

(require 'subr-x)

(use-package dash :demand t)

(use-package s :demand t)

(use-package f :demand t)

(use-package async
  :commands
  dired-async-mode
  async-bytecomp-package-mode
  async-start
  async-start-process
  async-let
  :config
  ;; (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

(provide 'm-library)

;;; lib.el ends here
