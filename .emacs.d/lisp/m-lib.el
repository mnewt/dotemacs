;;; m-lib.el --- Emacs Lisp Libraries -*- lexical-binding: t -*-

;;; Commentary:

;; Common libraries and associated functions.

;;; Code:

(require 'seq)

(require 'subr-x)

(use-package dash :demand t)

(use-package s :demand t)

(use-package f :demand t)

(use-package shut-up
  :commands shut-up
  :config
  (defun shut-up-advice-wrapper (symbol &rest args)
    "Wrap the call to function named SYMBOL with `shut-up'."
    (shut-up (apply symbol args)))

  (defmacro shut-up-advice-add (symbol)
    "Advise the function named SYMBOL to shut up."
    `(advice-add ,symbol :around #'shut-up-advice-wrapper))

  (defmacro shut-up-advice-add (symbol)
    "Remove shut up advice for the function named SYMBOL."
    `(advice-remove ,symbol #'shut-up-advice-wrapper)))

(use-package async
  :commands
  dired-async-mode
  async-bytecomp-package-mode
  async-start
  async-start-process
  async-let
  :config
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

(provide 'm-library)

;;; m-lib.el ends here
