;;; early-init.el --- Emacs early init -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;;; Code:

(defconst emacs-start-time (current-time))

;; (setq init-file-debug t)

(setq load-prefer-newer t)

;; Set Garbage Collection threshold to 1GB, run GC on idle.
(setq gc-cons-threshold 1073741824
      gc-cons-percentage 1.0)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
;; (setq package-enable-at-startup nil)

;; Faster to disable these here (before they've been initialized)
;; 
;; TODO: It seems they get enabled anyway and we have to disable them through
;; their packages. Is there a way to short circuit loading these packages?
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(tool-bar-mode   -1)
(menu-bar-mode   -1)
(scroll-bar-mode -1)
(tooltip-mode    -1)

;; Give the frame basic coloring while waiting for the theme to load. The main
;; purpose of this is to not blind me when it's dark. These colors are from
;; spacemacs-dark.
(set-face-attribute 'default nil :background "#1E2022" :foreground "#B1B2B1")
;; Default frame settings. This is actually maximized, not full screen.
(push '(fullscreen . maximized) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; These are good notes on optimizing startup performance:
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast

;; Unset `file-name-handler-alist' too (temporarily). Every file opened and
;; loaded by Emacs will run through this list to check for a proper handler for
;; the file, but during startup, it won’t need any of them.
(defvar m--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist m--file-name-handler-alist
                  gc-cons-percentage 0.1)
            (run-with-idle-timer 20 t #'garbage-collect)))

(provide 'early-init)

;;; early-init.el ends here

