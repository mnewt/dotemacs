;;; Init.el --- Emacs init file --- -*- lexical-binding: t -*-

;;; Commentary:
;; It's an Emacs init file. Relies on use-package for as much package
;; configuration as possible for its organization and performance
;; characteristics.

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

;;; Initial appearance settings so loading is not as jarring.

(when window-system
  ;; Give the frame basic coloring are waiting for the theme to load. These colors
  ;; are from spacemacs-dark and spacemacs-light.
  (if (equal 'dark (frame-parameter nil 'background-mode))
      (set-face-attribute 'default nil :background "#1E2022" :foreground "#B1B2B1")
    (set-face-attribute 'default nil :background "#fbf8ef" :foreground "#655370"))
  ;; Default frame settings. This is actually maximized, not full screen.
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;;; Package Management

;;;; use-package

(setq package-enable-at-startup nil
      package-user-dir "~/.emacs.d/packages/"
      package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/"))
      custom-file "~/.emacs.d/custom.el")

(eval-when-compile
  (require 'package)
  (package-initialize)
  (custom-set-variables
   '(use-package-always-ensure t)
   '(use-package-always-defer t)
   '(use-package-enable-imenu-support t)
   '(use-package-hook-name-suffix nil))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

;; (use-package quelpa-use-package
;;   :demand t
;;   :init
;;   (defvar quelpa-use-package-inhibit-loading-quelpa t)
;;   (defvar quelpa-update-melpa-p nil)
;;   :config
;;   (quelpa-use-package-activate-advice))

;;;; leaf

;; (prog1 "prepare leaf"
;;   (prog1 "package"
;;     (custom-set-variables
;;      '(package-archives '(("org"   . "https://orgmode.org/elpa/")
;;                           ("melpa" . "https://melpa.org/packages/")
;;                           ("gnu"   . "https://elpa.gnu.org/packages/"))))
;;     (package-initialize))

;;   (prog1 "leaf"
;;     (unless (package-installed-p 'leaf)
;;       (unless (assoc 'leaf package-archive-contents)
;;         (package-refresh-contents))
;;       (condition-case err
;;           (package-install 'leaf)
;;         (error
;;          (package-refresh-contents)     ; renew local melpa cache if fail
;;          (package-install 'leaf))))

;;     (leaf leaf
;;       :custom ((leaf-defaults . '(:ensure t))))

;;     (leaf leaf-keywords
;;       :ensure t
;;       :config (leaf-keywords-init)))

;;   (prog1 "optional packages for leaf-keywords"
;;     ;; optional packages if you want to use :hydra, :el-get,,,
;;     (leaf hydra :ensure t)
;;     (leaf el-get :ensure t
;;       :custom ((el-get-git-shallow-clone  . t)))))

;;; Benchmark init

;; (use-package benchmark-init
;;   :demand t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'emacs-startup-hook 'benchmark-init/deactivate))

(defvar elisp-directory "~/.emacs.d/lisp"
  "Local elisp configuration files go here.")

(add-to-list 'load-path elisp-directory)

(use-package use-package-git :demand t :ensure nil)

(use-package use-package-ensure-system-package :demand t)

;;; Emacs Lisp Extension Libraries

(require 'seq)
(require 'subr-x)
(use-package dash :demand t)
(use-package s :demand t)
(use-package f :demand t)
(use-package shut-up :commands shut-up)

;;; Private settings

(load "~/.emacs.d/m-private.el" t)

;;; Local Packages

(dolist-with-progress-reporter
    (p
     '(environment
       persist
       appearance
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
