;;; early-init.el --- Emacs early init -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;;; Code:

(defconst emacs-start-time (current-time))

;; Uncomment this to debug.
;; (setq init-file-debug t)
;; (setq messages-buffer-max-lines 100000)

;; If an `.el' file is newer than its corresponding `.elc', load the `.el'.
(setq load-prefer-newer t)

;; Set Garbage Collection threshold to 1GB, run GC on idle.
(setq gc-cons-threshold 1073741824
      gc-cons-percentage 0.6)

;; Write any customizations to a temp file so they are discarded.
(setq custom-file (make-temp-file "custom-" nil ".el"))

;; Faster to disable these here (before they've been initialized)
;; 
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Give the frame basic coloring while waiting for the theme to load. The main
;; purpose of this is to not blind me when it's dark by flashing a screen full
;; of white. These colors are from doom-one.
(set-face-attribute 'default nil :background "#282c34" :foreground "#bbc2cf")
;; Default frame settings. This is actually maximized, not full screen.
(push '(fullscreen . maximized) initial-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
          
;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; These are good notes on optimizing startup performance:
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast

;; Unset `file-name-handler-alist' too (temporarily). Every file opened and
;; loaded by Emacs will run through this list to check for a proper handler for
;; the file, but during startup, it won’t need any of them.
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old
                  gc-cons-percentage 0.1)
            (run-with-idle-timer 10 t #'garbage-collect)))

;;; Environment Variables

;; So that `comp' (Native Compilation) can find libgccjit and friends.
;; This is where Homebrew puts gcc libraries for GCC 10.
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/10")
;; Also /usr/local/bin/ needs to be on the PATH.  Both these things need to be
;; defined before the first package loads.
(setq exec-path '("/Applications/Wireshark.app/Contents/MacOS" "/Library/Frameworks/Mono.framework/Versions/Current/Commands" "/usr/local/share/dotnet" "/Users/mn/Applications/Emacs.app/Contents/MacOS" "/Users/mn/.private/bin" "/Users/mn/.emacs.d/bin" "/Users/mn/.bin" "/Users/mn/.local/bin" "/usr/local/opt/ruby/bin" "/usr/local/opt/llvm/bin" "/Users/mn/.gem/ruby/2.7.0/bin" "/Users/mn/Library/Python/3.8/bin" "/usr/local/opt/libxml2/bin" "/usr/local/opt/sqlite/bin" "/usr/local/opt/gnutls/bin" "/Library/TeX/texbin" "/usr/local/opt/texinfo/bin" "/usr/local/opt/coreutils/libexec/gnubin" "/usr/local/bin" "/usr/local/sbin" "/opt/X11/bin" "/Library/Apple/usr/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin"))
(setenv "PATH" (mapconcat #'identity exec-path ":"))
(setenv "PAGER" "cat")

(provide 'early-init)

;;; early-init.el ends here

