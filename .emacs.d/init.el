;;; Init.el --- Emacs init file --- -*- lexical-binding: t -*-

;;; Commentary:
;; It's an Emacs init file. Uses `straight.el' for package management and
;; use-package for as much package configuration as possible.

;;; Code:

;;; Top Level

;; These are good notes on optimizing startup performance:
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast

;; Set Garbage Collection threshold to 1GB, run GC on idle.
(setq gc-cons-threshold 1073741824)

(run-with-idle-timer 5 nil
                     (lambda ()
                       (run-with-idle-timer 20 t (lambda () (garbage-collect)))))

(setq load-prefer-newer t)

(with-eval-after-load 'gnutls
  (setq gnutls-verify-error t))

(with-eval-after-load 'nsm
  (setq network-security-level 'high))

(defvar elisp-directory "~/.emacs.d/lisp"
  "Local elisp configuration files go here.")

(add-to-list 'load-path elisp-directory)

;;; Package Management

;; Disable package.el initialization.
(setq package-enable-at-startup nil
      ;; don't add `custom-set-variables' block
      package--init-file-ensured t)

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

;; (defun m-straight-merge-all (&optional from-upstream)
;;   "Try to merge all packages from their primary remotes.
;; With prefix argument FROM-UPSTREAM, merge not just from primary
;; remotes but also from configured upstreams.

;; Do not merge packages listed in `m-pinned-packages'."
;;   (interactive "P")
;;   (straight-merge-all
;;    from-upstream
;;    (lambda (package)
;;      (not (member package m-straight-pinned-packages)))))

;; Packages in this list do not get updated when `update-packages' runs.
;; Therefore, they stay at their current version until manually updated in some
;; way, perhaps with `straight-merge-package'. See
;; https://github.com/raxod502/straight.el/issues/246#issuecomment-415085772.
;; (setq m-straight-pinned-packages
;;       '(org-mode))

(defun update-packages ()
  "Use straight.el to update all packages."
  (interactive)
  (straight-normalize-all)
  (straight-fetch-all)
  (straight-merge-all))

(use-package epkg
  :commands
  (epkg epkg-describe-package epkg-list-packages))

;; (use-package use-package-ensure-system-package)

;; (use-package benchmark-init
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;; Libraries

;; cl is assumed to be present in this config and some packages too.
;; (require 'cl)
(require 'seq)

(defun add-multiple-to-list (list items)
  "Run `add-to-list' for all ITEMS in the LIST."
  (seq-do (apply-partially #'add-to-list list) items))

;; These packages are used by many things

(use-package dash
  :commands
  (-map -filter -distinct -interpose))

(use-package s)

;;; Local Packages

(require 'm-appearance)
(require 'm-environment)
(require 'm-persist)
(require 'm-ui)
(require 'm-navigate)
(require 'm-help)
;; (require 'm-hydra)
(require 'm-search)
(require 'm-file)
(require 'm-vc)
(require 'm-shell)
(require 'm-eshell)
(require 'm-edit)
(require 'm-notes)
(require 'm-lisp)

(require 'm-modes)

(load "~/.emacs.d/m-private.el" t)

(load "~/.emacs.d/custom.el" t)

(provide 'init)

;;; init.el ends here
