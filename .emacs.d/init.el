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
    (message "Emacs has finished starting up in %.3f seconds." elapsed)))

(setq load-prefer-newer t)

(eval-after-load 'gnutls
  (defvar gnutls-verify-error t))

(with-eval-after-load 'nsm
  (defvar network-security-level 'high))

(defvar elisp-directory "~/.emacs.d/lisp"
  "Local elisp configuration files go here.")

(add-to-list 'load-path elisp-directory)

;;; Package Management

;; Disable package.el initialization.
(setq package-enable-at-startup nil)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; All external packages and many built in ones are configured using use-package.
(straight-use-package 'use-package)
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)

(defvar use-package-enable-imenu-support)
(setq use-package-enable-imenu-support t)

(eval-when-compile (require 'use-package))
(require 'bind-key)

;; https://github.com/raxod502/straight.el/issues/41
(defvar straight-check-for-modifications)
(setq straight-check-for-modifications 'live)

(defun update-packages ()
  "Use straight.el to update all packages."
  (interactive)
  (straight-normalize-all)
  (straight-fetch-all)
  (straight-merge-all))

(use-package use-package-ensure-system-package)

;; (use-package benchmark-init
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;; Emacs Lisp Extension Libraries

(require 'seq)
(require 'subr-x)
(use-package dash)
(use-package s)
(use-package f)

;;; Private settings

(load "~/.emacs.d/m-private.el" t)
(load "~/.emacs.d/custom.el" t)

;;; Local Packages

(require 'm-environment)
(require 'm-persist)
(require 'm-appearance)
(require 'm-ui)
(require 'm-navigate)
(require 'm-help)
(require 'm-hydra)
(require 'm-search)
(require 'm-file)
(require 'm-vc)
(require 'm-edit)
(require 'm-shell)
(require 'm-eshell)
(require 'm-notes)
(require 'm-lisp)
(require 'm-modes)

(provide 'init)

;;; init.el ends here
