;;; Init.el --- Emacs init file --- -*- lexical-binding: t -*-

;;; Commentary:
;; It's an Emacs init file. Uses `straight.el' for package management and
;; use-package for as much package configuration as possible.

;;; Code:

;;; Top Level

(defconst emacs-start-time (current-time))

;; These are good notes on optimizing startup performance:
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast

;; Set Garbage Collection threshold to 1GB, run GC on idle.
(setq gc-cons-threshold 1073741824)
(run-with-idle-timer 20 t #'garbage-collect)

;; Unset file-name-handler-alist too (temporarily). Every file opened and loaded
;; by Emacs will run through this list to check for a proper handler for the
;; file, but during startup, it won’t need any of them.
(defvar m--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda () (setq file-name-handler-alist m--file-name-handler-alist)))

(defun display-startup-echo-area-message ()
  "Display a message when Emacs finishes starting up."
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (defconst emacs-load-time elapsed)
    (message "Emacs started in %.3f seconds." elapsed)))

(setq load-prefer-newer t)

(with-eval-after-load 'gnutls
  (defvar gnutls-verify-error t))

(with-eval-after-load 'nsm
  (defvar network-security-level 'high))

;; Make the window dark while we are waiting for the theme to load.
(set-face-attribute 'default nil :background "#1E2022" :foreground "#B1B2B1")

;;; Package Management

(setq package-enable-at-startup nil
      package-user-dir "~/.emacs.d/packages/"
      package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/"))
      custom-file "~/.emacs.d/custom.el")

(eval-when-compile
  (require 'package)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-enable-imenu-support t))

(use-package use-package-ensure-system-package :defer 5)

;;; Emacs Lisp Extension Libraries

(use-package seq :demand t)
(use-package subr-x :demand t :ensure nil)
(use-package dash :demand t)
(use-package s :demand t)
(use-package f :demand t)
(use-package shut-up :demand t)

;;; Benchmark init

(use-package benchmark-init
  :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'emacs-startup-hook 'benchmark-init/deactivate))

;;; Private settings

(load "~/.emacs.d/m-private.el" t)

;;; Local Packages

(defvar elisp-directory "~/.emacs.d/lisp"
  "Local elisp configuration files go here.")

(dolist-with-progress-reporter
    (p
     '(environment
       persist
       appearance
       ui
       help
       navigate
       search
       file
       net
       vc
       edit
       shell
       eshell
       notes
       lisp
       modes)

     (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
       (defconst emacs-load-time elapsed)
       (message "Emacs loaded packages in %.3f seconds." elapsed)
       elapsed))
    "Emacs is starting... "
  (load-file (format "%s/m-%s.el" elisp-directory (symbol-name p))))

(provide 'init)

;;; init.el ends here
