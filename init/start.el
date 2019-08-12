;;; start.el --- Startup Stuff -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton


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

;; Things that run at the very beginning of Emacs startup

;;; Code:

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
  ;; mode-line-format will be set in `m-appearance'.
  ;; (setq mode-line-format "")
  ;; Default frame settings. This is actually maximized, not full screen.
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(provide 'start)

;;; start.el ends here
