;;; init.el --- Emacs init file --- -*- lexical-binding: t -*-

;;; Commentary:

;; It's an Emacs init file. Relies on heavily on use-package for its
;; organization and performance features.

;;; Code:

;;;; Start

;; Things that run at the very beginning of Emacs startup

(defconst emacs-start-time (current-time))

;; (setq debug-on-error t)

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

;;;;; Initial appearance settings

;; We set them here so loading is not as jarring.

(when window-system
  ;; Give the frame basic coloring are waiting for the theme to load. These colors
  ;; are from spacemacs-dark and spacemacs-light.
  (if (equal 'dark (frame-parameter nil 'background-mode))
      (set-face-attribute 'default nil :background "#1E2022" :foreground "#B1B2B1")
    (set-face-attribute 'default nil :background "#fbf8ef" :foreground "#655370"))
  ;; Default frame settings. This is actually maximized, not full screen.
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))


;;;; Variables

;; Top level user variables

(defvar code-directory (if (file-exists-p "~/code") "~/code" "~")
  "Default code project container directory.")

(cd code-directory)

(defvar journal-directory "~/org/journal"
  "Location of journal entries.")

;;;; Environment

;; Set up Operating System and Environment details.

(require 'cl-seq)

;; Path
(defvar path-default (if (eq system-type 'windows-nt)
                         '("C:/bin" "C:/Program Files/Emacs/bin")
                       nil)
  "Defines a list of path entries to add by default.")

(defvar path-user nil
  "Defines a list of path entries to add to all systems.")

(defun source-sh (filename)
  "Sources FILENAME using the user's login shell.
Update environment variables from a shell source file."
  (interactive "fSource file: ")
  (message "Sourcing environment from `%s'..." filename)
  (with-temp-buffer
    (shell-command (format "diff -u <(true; export) <(source %s; export)" filename) '(4))
    (let ((envvar-re "declare -x \\([^=]+\\)=\\(.*\\)$"))
      ;; Remove environment variables
      (while (search-forward-regexp (concat "^-" envvar-re) nil t)
        (let ((var (match-string 1)))
          ;; (message "%s" (prin1-to-string `(setenv ,var nil)))
          (setenv var nil)))
      ;; Update environment variables
      (goto-char (point-min))
      (while (search-forward-regexp (concat "^+" envvar-re) nil t)
        (let ((var (match-string 1))
              (value (read (match-string 2))))
          ;; (message "%s" (prin1-to-string `(setenv ,var ,value)))
          (setenv var value)))))
  (message "Sourcing environment from `%s'... done." filename))

(defun path-add (&rest paths)
  "Add PATHS to the OS and Emacs executable search paths."
  (let* ((old-path (split-string (getenv "PATH") path-separator))
         new-path)
    (dolist (path (append paths old-path))
      (setq path (expand-file-name path))
      (when (file-directory-p path) (cl-pushnew path new-path :test #'string=)))
    (setenv "PATH" (mapconcat #'identity new-path path-separator))
    (setq exec-path new-path)))

(defun path-reset ()
  "Set path variables correctly for Linux, macOS, or Windows."
  (apply #'path-add (append path-user path-default)))

(source-sh "~/.env")
(source-sh "~/.bin/start-ssh-agent")
(path-reset)

(defvar os-open-file-executable nil
  "The executable used to open files in the host OS GUI.")

(defun config-unix ()
  "Configure Emacs for common Unix (Linux and macOS) settings."
  nil)

(defun config-linux ()
  "Configure Emacs for Linux."
  (config-unix))

(defun config-macos ()
  "Configure Emacs for macOS."
  (config-unix)
  (setq ns-alternate-modifier 'meta
        ns-right-alternate-modifier 'none
        ns-command-modifier 'super
        ns-right-command-modifier 'left
        ns-control-modifier 'control
        ns-right-control-modifier 'left
        ns-function-modifier 'hyper
        ;; Open files from Finder in same frame.
        ns-pop-up-frames nil
        os-open-file-executable "open")
  ;; Use system trash
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash"))

(defvar w32-pass-lwindow-to-system)
(defvar w32-lwindow-modifier)
(defvar w32-pass-rwindow-to-system)
(defvar w32-rwindow-modifier)

(defun config-windows ()
  "Configure Emacs for Windows."
  (setq w32-pass-lwindow-to-system nil
        w32-lwindow-modifier 'super
        w32-pass-rwindow-to-system nil
        w32-rwindow-modifier 'super
        os-open-file-executable "explorer"))

;; OS specific configuration
(pcase system-type
  ('darwin (config-macos))
  ('gnu/linux (config-linux))
  ('windows-nt (config-windows))
  ('cygwin (config-windows)))


;;;; Package

;; Emacs Package Management

;;;;; use-package

(defvar elisp-directory "~/.emacs.d/lisp"
  "Drop package files here to put them on the `load-path'.")

(add-to-list 'load-path elisp-directory)

(setq package-enable-at-startup nil
      package-user-dir "~/.emacs.d/packages/"
      package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/"))
      package-archive-priorities '(("org" . 30)
                                   ("melpa" . 20)
                                   ("elpa" . 10))
      custom-file "~/.emacs.d/custom.el")

;; https://github.com/nilcons/emacs-use-package-fast
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        ;; (require 'package)
        (package-initialize)
        ;; use-package customizations
        (custom-set-variables
         '(use-package-always-ensure t)
         '(use-package-always-defer t)
         '(use-package-enable-imenu-support t)
         '(use-package-hook-name-suffix nil))
        ;; Install use-package if not installed yet.
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        ;; (require 'use-package)
        (custom-set-variables
         '(use-package-always-ensure t))
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse (apply #'nconc
                           ;; Only keep package.el provided loadpaths.
                           (mapcar #'(lambda (path)
                                       (if (string-prefix-p package-user-dir-real path)
                                           (list path)
                                         nil))
                                   load-path))))))

;; (use-package benchmark-init
;;   :demand t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'emacs-startup-hook 'benchmark-init/deactivate))

(add-to-list 'load-path elisp-directory)

(use-package use-package-git :demand t :ensure nil)

(use-package system-packages
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list
     'system-packages-supported-package-managers
     '(choco .
             ((default-sudo . t)
              (install . "choco install")
              (search . "choco search")
              (uninstall . "choco uninstall")
              (update . "choco upgrade")
              (clean-cache . "choco optimize")
              (log . "type C:\\ProgramData\\chocolatey\\logs\\chocolatey.log")
              (get-info . "choco info --local-only")
              (get-info-remote . "choco info")
              (list-files-provided-by . nil)
              (verify-all-packages . nil)
              (verify-all-dependencies . nil)
              (remove-orphaned . nil)
              (list-installed-packages . "choco list --local-only")
              (list-installed-packages-all . "choco list --local-only --include-programs")
              (list-dependencies-of . nil)
              (noconfirm . "-y"))))
    (setq system-packages-package-manager 'choco)))

(use-package use-package-ensure-system-package :demand t)

;;;;; use-package-list

;; Create a list of all packages defined with a `use-package' directive.

(add-to-list 'use-package-keywords :list)
(add-to-list 'use-package-defaults '(:list t t))

(defvar use-package-list nil
  "Packages defined by `use-package'.")

(defun use-package-normalize/:list (_name _keyword _args)
  "Serves no function; here only as boilerplate."
  (list t))

(defun use-package-handler/:list (name _keyword _ensure rest state)
  "Add the package NAME to the list.
REST and STATE are passed to `use-package-process-keywords'."
  (let* ((body (use-package-process-keywords name rest state)))
    (add-to-list 'use-package-list name)
    body))

;; TODO Develop a better way to ensure only currently configured packages are
;; installed. Use `use-package-list'.
;; See https://yoo2080.wordpress.com/2014/05/16/how-to-list-emacs-package-dependencies/
(defun package-delete-all ()
  "Delete all packages in `package-user-dir'.

We do this to get rid of any stale packages and force a reinstall
on the next startup."
  (interactive)
  (shell-command (concat "rm -rf " package-user-dir)))

(defvar package-dependencies-alist nil
  "List of packages and their dependencies.")

(defun package-refresh-dependencies-alist ()
  "Refresh `package-dependencies-alist'."
  (setq package-dependencies-alist
        (cl-loop for pkg in package-activated-list
                 for pkg-vec = (cadr (assq pkg package-alist))
                 when pkg-vec
                 collect (cons pkg
                               (cl-loop for req in (package-desc-reqs pkg-vec)
                                        for req-name = (car req)
                                        when (memq req-name package-activated-list)
                                        collect req-name))))
  package-dependencies-alist)

(defun find-duplicates (list)
  "Get the duplicate elements from LIST."
  (cl-loop for (item . count) in
           (let ((counts '())
                 place)
             (dolist (el list)
               (setq place (assoc el counts))
               (if place
                   (cl-incf (cdr place))
                 (push (cons el 1) counts)))
             counts)
           if (> count 1)
           collect item))

(defun package-delete-unused ()
  "Delete unused packages."
  (interactive)
  (let* ((default-directory package-user-dir)
         ;; Could do this from `package-alist' / package-desc but what we really
         ;; care about is what is on disk, so go straight to it.
         installed-package-alist duplicates)
    (dolist (dir (file-expand-wildcards "*-*"))
      (when (file-directory-p dir)
        (push (cons (intern (replace-regexp-in-string "-[0-9\\.]+\\'" "" dir))
                    dir)
              installed-package-alist)))
    (setq duplicates
          (mapcar (lambda (dup) (sort (seq-filter
                                       (lambda (e) (equal dup (car e)))
                                       installed-package-alist)
                                      (lambda (a b)
                                        (string-greaterp (cdr a) (cdr b)))))
                  (find-duplicates (mapcar #'car installed-package-alist))))
    ;;     (dolist (dup duplicates))))
    ;; \      (pp (concat "rm -rf " (string-join old-files " "))))))
    ;; TODO: Delete all but newest duplicate.

    (pp (car duplicates))))

(defun emacs-startup-message ()
  "Display a message after Emacs startup."
  (defvar use-package-git--packages)
  (defconst emacs-load-time
    (float-time (time-subtract (current-time) emacs-start-time)))

  (message "Emacs loaded %d packages in %.1f seconds."
           (+ (length package-activated-list) (length use-package-git--packages))
           emacs-load-time))

(add-hook 'emacs-startup-hook #'emacs-startup-message)

(use-package paradox
  :custom
  (paradox-automatically-star t)
  :commands
  paradox-list-packages
  paradox-upgrade-packages)


;;;; Libraries

;; Common libraries and associated functions.

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

;;;; Bindings

;; Define base key bindings

;; Define "M-m" as a prefix key.
(bind-key "M-m" nil)
(define-prefix-command 'm-map)
(defvar m-map (make-sparse-keymap "M"))

;; Prefix key "M-m i": Insert commands.
(define-prefix-command 'm-insert-map)
(global-set-key (kbd "M-m i") 'm-insert-map)
(defvar m-insert-map (make-sparse-keymap "M-Insert"))

;; Prefix key "M-m w": Window configurations.
(define-prefix-command 'm-window-map)
(global-set-key (kbd "M-m w") 'm-window-map)
(defvar m-window-map (make-sparse-keymap "M-Window"))

;; Prefix key "M-m f": File operations.
(define-prefix-command 'm-file-map)
(global-set-key (kbd "M-m f") 'm-file-map)
(defvar m-file-map (make-sparse-keymap "M-File"))

;; Prefix key "M-m h": Help & Hydra.
(define-prefix-command 'm-help-map)
(global-set-key (kbd "M-m h") 'm-help-map)
(defvar m-help-map (make-sparse-keymap "M-Help"))

;; Prefix key "M-m t": Toggles
(define-prefix-command 'm-toggle-map)
(global-set-key (kbd "M-m t") 'm-toggle-map)
(defvar m-toggle-map (make-sparse-keymap "M-Toggle"))

;; Prefix key "M-m s": Search
(define-prefix-command 'm-search-map)
(global-set-key (kbd "M-m s") 'm-search-map)
(defvar m-search-map (make-sparse-keymap "M-Search"))

;; Key bindings to make moving between Emacs and other appliations a bit less
;; jarring. These are mostly based on macOS defaults but an effor has been made
;; to work on Windows and Linux. That is why there are multiple bindings for
;; many commands. They can be overridden by the OS specific configurations
;; below.

(bind-keys
 ("s-o" . find-file)
 ("s-O" . find-file-other-window)
 ("s-s" . save-buffer)
 ("s-S" . write-file)
 ("s-q" . save-buffers-kill-emacs)
 ("s-z" . undo)
 ("C-z" . undo)
 ("s-a" . mark-whole-buffer)
 ("s-g" . isearch-repeat-forward)
 ("s-G" . isearch-repeat-backward)
 ("C-S-s" . isearch-forward-symbol-at-point)
 ("s-l" . select-current-line)
 ("C-S-L" . select-current-line)
 ("M-o" . other-window)
 ("s-b" . switch-to-buffer)
 ("s-B" . switch-to-buffer-other-window)
 ("s-\`" . other-frame)
 ("C-\`" . other-frame)
 ("s-w" . delete-window)
 ("s-W" . delete-other-windows)
 ("s-C-w" . delete-frame)
 ("s-h" . ns-do-hide-emacs)
 ("s-H" . ns-do-hide-others)
 ("C-c U" . revert-buffer)
 ("s-<return>" . eval-last-sexp)
 ("s-RET" . eval-last-sexp))


;;;; Persistence

;; Persist Emacs session data.

;; Store all backup and autosave files in their own directory since it is bad to
;; clutter project directories.
(with-eval-after-load 'files
  (setq backup-directory-alist '((".*" . "~/.emacs.d/backup"))
        ;; Automatic backup file housekeeping.
        kept-new-versions 10
        kept-old-versions 4
        delete-old-versions t
        ;; Don't clobber symlinks.
        backup-by-copying t
        ;; Don't break multiple hardlinks.
        backup-by-copying-when-linked t
        ;; Use version numbers for backup files.
        version-control t
        ;; Backup even if file is in vc.
        vc-make-backup-files t
        auto-save-list-file-prefix "~/.emacs.d/autosave/"
        auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t))
        ;; Don't create `#file-name' lockfiles in $PWD. Lockfiles are useful but it
        ;; generates too much activity from tools watching for changes during
        ;; development.
        create-lockfiles nil
        ;; Increase undo limit to 5MB per buffer.
        undo-limit 5242880))

(use-package saveplace
  :defer 1
  :commands
  save-place-mode
  :config
  (save-place-mode))

(use-package recentf
  :defer 1
  :commands
  recentf-save-list
  recentf-cleanup
  recentf-mode
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'never)
  :config
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))

  (defun grep-recent-files (filepattern pattern)
    (interactive "sFiles regexp: \nsSearch regexp: ")
    (let ((files (if filepattern
                     (cl-remove-if-not (lambda (item) (string-match filepattern item))
                                       recentf-list)
                   recentf-list))
          (limit 50))
      (if (> (length files) limit)
          (seq-subseq files 0 limit))

      (let* ((tempfile (make-temp-file "emacs"))
             (orig compilation-finish-functions))
        (add-to-list 'compilation-finish-functions
                     (lambda (_buf _result)
                       (setq font-lock-keywords-case-fold-search t)
                       (highlight-regexp pattern 'hi-yellow)
                       (delete-file tempfile)
                       (setq compilation-finish-functions orig)))

        (write-region  (mapconcat 'identity files (char-to-string 0))
                       nil tempfile)

        (grep (format "%s %s | xargs -0 grep -n -i \"%s\" "
                      (if (eq system-type 'windows-nt)
                          "type"
                        "cat")

                      (if (eq system-type 'windows-nt)
                          (replace-regexp-in-string "/" "\\\\" tempfile)
                        tempfile)

                      pattern)))))

  (recentf-mode)
  :hook
  (dired-mode-hook . recentf-add-dired-directory))

(use-package autorevert
  :defer 2
  :custom
  ;; Work in Dired.
  (global-auto-revert-non-file-buffers t)
  ;; Don't print auto revert messages.
  (auto-revert-verbose nil)
  :commands
  global-auto-revert-mode
  :config
  (global-auto-revert-mode))

(use-package savehist
  :defer 1
  :commands
  savehist-mode
  :custom
  (savehist-autosave-interval 60)
  (history-length 200)
  (history-delete-duplicates t)
  (savehist-additional-variables
   '(kill-ring
     search-ring
     regexp-search-ring
     file-name-history
     magit-read-rev-history
     read-expression-history
     command-history
     extended-command-history
     ivy-history))
  :config
  (savehist-mode))

(use-package desktop
  :demand t
  :custom
  (desktop-dirname "~/.emacs.d")
  :config
  (setq desktop-globals-to-save
        (append desktop-globals-to-save
                '(kill-ring
                  read-expression-history
                  theme-current-theme)))
  :bind
  (:map m-map
        ("d" . desktop-save-mode)))

(use-package persistent-scratch
  :defer 1
  :config
  (persistent-scratch-setup-default))


;;;; Private

;; If it exists, load the private configuration file.

(load "~/.emacs.d/private.el" t t nil t)


;;;; Appearance

;; Set up visual UI and theme stuff.

(custom-set-variables '(inhibit-splash-screen t))

;; Beeping is REALLY NOT OK
(setq visible-bell t
      ;; Show keystrokes right away.
      echo-keystrokes 0.01)

(when window-system
  ;; GUI Configuration
  (progn
    (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
    (when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
    (setq frame-resize-pixelwise t
          ;; We don't set a frame title because Emacs on macOS renders the
          ;; frame title face terribly. No rendering is better than terrible
          ;; rendering. Also, it is clean and nice this way.
          frame-title-format nil
          ;; No icon in the titlebar
          ns-use-proxy-icon nil
          ;; Smoother and nicer scrolling
          scroll-margin 6
          scroll-step 1
          scroll-conservatively 10000
          scroll-preserve-screen-position 1
          auto-window-vscroll nil
          ;; No GUI dialogs
          use-dialog-box nil)

    ;; Blinking is NOT OK
    (blink-cursor-mode -1)

    (with-eval-after-load 'face-remap
      (eval-when-compile
        (defvar text-scale-mode-step))
      (setq text-scale-mode-step 1.1))

    (defun some-font (font-list)
      "Return a 'Font-Size' combination from FONT-LIST.

The first one which is available in the current environment is
returned."
      (cl-some (lambda (font-pitch)
                 (string-match "\\`\\([^-]+\\)" font-pitch)
                 (when (member (substring font-pitch
                                          (match-beginning 1)
                                          (match-end 1))
                               (font-family-list))
                   font-pitch))
               font-list))

    ;; Set default fonts.
    (defvar m-fixed-pitch-font
      (some-font '("Input-14" "Monaco-13" "Lucida Console-12" "DejaVu Sans-12"
                   "Inconsolata-14"))
      "The default font to use for fixed pitch applications.")

    (defvar m-variable-pitch-font
      (some-font '("Avenir-17" "Calibri" "Helvetica Neue" "Helvetica" "Georgia-15"))
      "The default font to use for variable pitch applications.")

    (dolist (face '(default fixed-pitch))
      (set-face-font face m-fixed-pitch-font))

    (set-face-font 'variable-pitch m-variable-pitch-font)

    (add-hook 'text-mode-hook 'variable-pitch-mode)

    ;; Wrap text at the end of a line like a word processor.
    (add-hook 'text-mode-hook #'turn-on-visual-line-mode)

    (with-eval-after-load 'mwheel
      (setq mouse-wheel-follow-mouse 't
            mouse-wheel-scroll-amount '(1 ((shift) . 1))))

    (use-package pixel-scroll
      :defer 1
      :ensure nil
      :commands
      pixel-scroll-mode
      :config
      (pixel-scroll-mode))

    (use-package mixed-pitch
      :hook
      (text-mode-hook . mixed-pitch-mode))

    (use-package fontify-face
      :hook
      (emacs-lisp-mode-hook . fontify-face-mode))))

(unless (and window-system (eq system-type 'darwin))
  (menu-bar-mode -1))

(use-package spacemacs-common
  :ensure spacemacs-theme)

(use-package fiat-color
  :demand t
  :ensure nil
  :custom
  (fiat-lux-theme 'spacemacs-light)
  (fiat-nox-theme 'spacemacs-dark)
  (fiat-themes '((spacemacs-light) (spacemacs-dark)))
  (fiat-specs-common '((cursor ((t :background "magenta")))))
  :config
  (fiat-theme)
  (fiat-mode-line-mode)
  :commands
  (fiat-theme fiat-lux fiat-nox fiat-mode-line-mode)
  :bind
  ("C-M-s-S-t" . fiat-theme-choose)
  ("C-M-s-t" . fiat)
  (:map m-toggle-map
        ("t" . fiat)
        ("c" . fiat-show-flycheck-toggle)
        ("l" . fiat-show-line-and-column-toggle)))

(use-package window-highlight
  :demand t
  :if (and window-system (>= emacs-major-version 27))
  :git "https://github.com/dcolascione/emacs-window-highlight"
  :commands
  window-highlight-mode
  :config
  (window-highlight-mode))

(use-package hl-line
  :defer 1
  :commands
  global-hl-line-mode
  :config
  (global-hl-line-mode))

;; (use-package flash-thing
;;   :defer 6
;;   :git "git@github.com:mnewt/flash-thing.git"
;;   :custom
;;   (ring-bell-function #'flash-window)
;;   :commands
;;   (flash-thing-mode flash-window)
;;   :config
;;   (flash-thing-mode))

(use-package page-break-lines
  :demand t
  :commands
  global-page-break-lines-mode
  :config
  (global-page-break-lines-mode))

(use-package darkroom
  :bind
  (:map m-toggle-map
        ("d" . darkroom-mode)))

(use-package hl-todo
  :defer 6
  :custom
  (hl-todo-keyword-faces
   '(("TODO" . "magenta")
     ("FIXME" . "magenta")
     ("\\?\\?\\?+" . "magenta")
     ("WIP" . "hot pink")
     ("NOTE" . "blue")
     ("KLUDGE" . "orange")
     ("HACK" . "orange")
     ("TEMP" . "orange")
     ("XXX+" . "orange")
     ("NEXT" . "lime green")
     ("DONE" . "gray")))
  :commands
  global-hl-todo-mode
  :config
  (global-hl-todo-mode)
  :bind
  ("M-s h i" . hl-todo-insert)
  ("M-s h p" . hl-todo-previous)
  ("M-s h n" . hl-todo-next)
  ("M-s h o" . hl-todo-occur))

(defun font-lock-reset (&optional keywords)
  "Reload font-locking for the buffer with KEYWORDS."
  (interactive)
  (setq font-lock-keywords nil)
  (let ((mode major-mode))
    (fundamental-mode)
    (funcall mode))
  (when keywords
    (font-lock-add-keywords major-mode keywords)
    (font-lock-refresh-defaults)
    (font-lock-flush)
    (font-lock-ensure)))

(use-package font-lock-studio
  :commands
  (font-lock-studio))


;;;; Help

;; Help and Documentation lookup

;; Change yes/no prompts to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show key bindings when the command is executed from `M-x'.
(with-eval-after-load 'simple
  (setq suggest-key-bindings 5))

;; Enable all commands without silly warnings.
(with-eval-after-load 'novice
  (setq disabled-command-function nil))

(use-package help-at-pt
  :defer 2
  :custom
  (help-at-pt-display-when-idle t)
  :commands
  help-at-pt-set-timer
  :config
  (help-at-pt-set-timer))

(use-package helpful
  :bind
  ("C-h ." . helpful-at-point)
  ("C-h f" . helpful-callable)
  ("C-h c" . helpful-command)
  ("C-h F" . helpful-function)
  ("C-h k" . helpful-key)
  ("C-h M" . helpful-macro)
  ("C-h M-s" . helpful-symbol)
  ("C-h v" . helpful-variable))

(with-eval-after-load 'shr
  (eval-when-compile
    (defvar shr-color-visible-luminance-min)
    (defvar shr-color-visible-distance-min)
    (defvar shr-use-colors))
  (setq shr-color-visible-luminance-min 60
        shr-color-visible-distance-min 5
        shr-use-colors nil))

(use-package eldoc
  :defer 2
  :commands
  eldoc-add-command
  global-eldoc-mode
  :config
  (eldoc-add-command #'company-select-next
                     #'company-select-previous
                     #'keyboard-quit
                     #'outshine-self-insert-command)
  (global-eldoc-mode))

(use-package which-key
  :demand t
  :defer 2
  :commands
  which-key-mode
  :config
  (which-key-mode)
  :bind
  ("M-s-h" . which-key-show-top-level))

(use-package man
  :custom
  ;; Make the manpage the current buffer in the other window
  (Man-notify-method 'aggressive)
  :config
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)
  :bind
  ("C-h M-m" . man))

(defun tramp-aware-woman (man-page-path)
  "Open a remote man page at MAN-PAGE-PATH via TRAMP."
  (interactive)
  (let ((dir default-directory))
    (woman-find-file
     (if (file-remote-p dir)
         (let ((vec (tramp-dissect-file-name dir)))
           (tramp-make-tramp-file-name
            (tramp-file-name-method vec)
            (tramp-file-name-user vec)
            (tramp-file-name-host vec)
            man-page-path))
       man-page-path))))

(use-package eg
  :git "https://github.com/mnewt/eg.el"
  :ensure-system-package
  (eg . "pip install eg")
  :bind
  ("C-h e" . eg))

(defvar dash-docs-docsets-path "~/.config/docsets"
  "Local path to save docsets.")

(make-directory dash-docs-docsets-path t)

(defun dash-docs-installed-docsets ()
  "Return a list of the currently installed docsets."
  (mapcar (lambda (f) (string-trim-right f ".docset"))
          (directory-files dash-docs-docsets-path nil "[^.]*\.docset")))

(use-package counsel-dash
  :ensure-system-package sqlite3
  :init
  (defun dash-docs-update-docsets-var (&optional _)
    "Update `dash-docs-common-docsets' variable."
    (interactive (list t))
    (setq dash-docs-common-docsets (dash-docs-installed-docsets)))

  (advice-add 'dash-docs-install-docset :after #'dash-docs-update-docsets-var)
  (advice-add 'dash-docs-install-user-docset :after #'dash-docs-update-docsets-var)

  (defun dash-docs-update-all-docsets ()
    "Update all official and unofficial docsets."
    (interactive)
    (seq-doseq (d (dash-docs-installed-docsets))
      (cond
       ((memq d (dash-docs-official-docsets))
        (dash-docs-install-docset d))
       ((memq d (dash-docs-unofficial-docsets))
        (dash-docs-install-user-docset d))
       (t
        (message "Skipping manually installed docset: %s..." d))))
    (dash-docs-update-docsets-var))

  (defun eww-other-window (url)
    "Fetch URL and render the page.

Open the `eww' buffer in another window."
    (interactive
     (let* ((uris (eww-suggested-uris))
            (prompt (concat "Enter URL or keywords"
                            (if uris (format " (default %s)" (car uris)) "")
                            ": ")))
       (list (read-string prompt nil 'eww-prompt-history uris))))
    (require 'eww)
    (switch-to-buffer-other-window (current-buffer))
    (eww url t))

  :custom
  (dash-docs-docsets-path "~/.config/docsets")
  (dash-docs-browser-func #'eww-other-window)
  (dash-docs-common-docsets (dash-docs-installed-docsets))
  (dash-docs-enable-debugging nil)
  :commands
  (counsel-dash counsel-dash-at-point dash-docs-install-docset
                dash-docs-official-docsets dash-docs-unofficial-docsets)
  :bind
  ("M-s-l" . counsel-dash)
  ("C-h C-d" . counsel-dash)
  ("M-s-." . counsel-dash-at-point))

(use-package lv
  :commands
  lv-message
  lv-window
  lv-delete-window)

(use-package hydra
  :defer 2
  :config
  (defun hydra-move-splitter-left (arg)
    "Move window splitter left by ARG characters."
    (interactive "p")
    (if (windmove-find-other-window 'right)
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right by ARG characters."
    (interactive "p")
    (if (windmove-find-other-window 'right)
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up by ARG characters."
    (interactive "p")
    (if (windmove-find-other-window 'up)
        (enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down by ARG characters."
    (interactive "p")
    (if (windmove-find-other-window 'up)
        (shrink-window arg)
      (enlarge-window arg)))

  (defhydra hydra-window (:hint nil)
    "
MOVE WINDOW^   _h_ left          _j_ down                    _k_ up             _l_ right
MOVE BUFFER^   _←_ left          _↓_ down                    _↑_ up             _→_ right
SPLIT^         _V_ vertical      _H_ horizontal              _u_ undo           _r_ redo
SIZE^          _b_ thinner       _n_ taller                  _p_ shorter        _f_ wider                 _B_ balance
DELETE^        _d_ kill buffer   _D_ kill buffer and window  _w_ delete window  _W_ delete other windows
              _q_ quit
"
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("<left>" buf-move-left)
    ("<down>" buf-move-down)
    ("<up>" buf-move-up)
    ("<right>" buf-move-right)
    ("V" (lambda () (interactive) (split-window-right) (windmove-right)))
    ("H" (lambda () (interactive) (split-window-below) (windmove-down)))
    ("u" (progn (winner-undo) (setq this-command 'winner-undo)))
    ("r" winner-redo)
    ("b" hydra-move-splitter-left)
    ("n" hydra-move-splitter-down)
    ("p" hydra-move-splitter-up)
    ("f" hydra-move-splitter-right)
    ("B" balance-windows)
    ("d" kill-current-buffer)
    ("D" kill-buffer-and-window)
    ("w" delete-window)
    ("W" delete-other-windows)
    ("q" nil))

  (defhydra hydra-move (:hint nil)
    "
hydra-move: [_n_ _N_ _p_ _P_ _v_ _V_ _u_ _d_] [_f_ _F_ _b_ _B_ _a_ _A_ _e_ _E_] [_,_ _._ _l_ _c_] _q_"
    ("n" next-line)
    ("N" scroll-down-margin)
    ("p" previous-line)
    ("P" scroll-up-margin)
    ("v" scroll-up-command)
    ("V" scroll-down-command)
    ("u" scroll-window-up)
    ("d" scroll-window-down)
    ("f" forward-char)
    ("F" forward-word)
    ("b" backward-char)
    ("B" backward-word)
    ("a" mwim-beginning-of-code-or-line)
    ("A" beginning-of-defun)
    ("e" mwim-end-of-code-or-line)
    ("E" end-of-defun)
    ("," beginning-of-buffer)
    ("." end-of-buffer)
    ("l" recenter-top-bottom)
    ("c" goto-last-change)
    ("q" nil))

  :commands
  (defhydra hydra-default-pre hydra-keyboard-quit
    hydra--call-interactively-remap-maybe hydra-show-hint
    hydra-set-transient-map
    hydra-window/body)
  :bind
  ("C-s-v" . hydra-move/body)
  ("C-c w" . hydra-window/body))

(use-package counsel-ffdata
  :custom
  (counsel-ffdata-database-path
   "/Users/mn/Library/Application Support/Firefox/Profiles/pmgg09p8.dev-edition-default/places.sqlite")
  :bind
  ("C-c F h" . counsel-ffdata-firefox-history)
  ("C-c F b" . counsel-ffdata-firefox-bookmarks))

(use-package counsel-web
  :ensure nil
  :load-path "git/counsel-web"
  :bind
  (:map m-search-map
        ("w" . counsel-web-search)))

(use-package stack-answers
  :ensure nil
  :hook
  (stack-answers-mode-hook . mixed-pitch-mode)
  :bind
  (:map m-search-map
        ("a" . stack-answers)))

(bind-keys
 ("C-h C-i" . elisp-index-search)
 ("C-h M-i" . info-apropos)
 :map Info-mode-map
 ("j" . next-line)
 ("k" . previous-line))

(seq-doseq (m '(Info-mode-map help-mode-map))
  (bind-keys :map (symbol-value m)
             ("j" . next-line)
             ("k" . previous-line)))


;;;; Org

;; Make package.el install Org from repo instead of using the built in version.
(assq-delete-all 'org package--builtins)
(unless (file-expand-wildcards (concat package-user-dir "/org-[0-9]*"))
  (package-install (elt (cdr (assoc 'org package-archive-contents)) 0)))
;; We have to be really sure something doesn't load `org' before this, or we get
;; the version that ships with Emacs.

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-directory "~/org")
  ;; Indent text according to the outline structure (`org-indent-mode')
  (org-startup-indented t)
  ;; Quit adding 2 spaces to source block
  (org-edit-src-content-indentation 0)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  ;; Insert a row in tables
  (org-special-ctrl-o t)
  ;; Tab in source blocks should act like in major mode
  (org-src-tab-acts-natively t)
  ;; Code highlighting in code blocks
  (org-src-fontify-natively t)
  (org-hide-leading-stars t)
  (org-export-with-section-numbers nil)
  ;; Customize todo keywords
  (org-todo-keywords '((sequence "TODO(t)" "WIP(w)" "DONE(d!)")))
  (org-todo-keyword-faces '(("TODO" (:foreground "magenta" :weight bold))
                            ("WIP" (:foreground "hot pink" :weight bold))
                            ("DONE" (:foreground "gray" :weight bold))))
  (org-catch-invisible-edits 'show-and-error)
  (org-capture-templates
   `(("t" "TODO" entry
      (file+headline ,(expand-file-name "TODO.org" org-directory) "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("n" "Note" entry
      (file+headline ,(expand-file-name "TODO.org" org-directory) "Tasks")
      "* %?\n  %i\n  %a")
     ("m" "TODO respond to email" entry
      (file ,(expand-file-name "TODO.org" org-directory))
      "* TODO %^{Description}\n%A\n%?\n")))
  ;; Don't prompt to confirm if I want to evaluate a source block
  (org-confirm-babel-evaluate nil)
  (org-startup-with-inline-images "inlineimages")
  (org-image-actual-width 500)
  :commands
  org-todo
  org-entry-get
  org-sort-entries
  org-map-entries
  org-capture
  org-capture-refile
  :config

  (defun org-search-org-directory ()
    "Search ~/org using `counsel-rg'."
    (interactive)
    (let ((default-directory org-directory))
      (counsel-rg)))

  (defun org-todo-todo ()
    "Create or update Org todo entry to TODO status."
    (interactive)
    (org-todo "TODO"))

  (defun org-todo-to-int (todo)
    "Get the number of the TODO based on its status."
    (car (cl-remove
          nil
          (mapcar (lambda (keywords)
                    (let ((todo-seq
                           (mapcar (lambda (x) (car (split-string  x "(")))
                                   (cdr keywords))))
                      (cl-position-if (lambda (x) (string= x todo)) todo-seq)))
                  org-todo-keywords))))

  (defun org-sort-entries--todo-status-key ()
    "Sort Org TODO entries by their status."
    (let* ((todo-max (apply #'max (mapcar #'length org-todo-keywords)))
           (todo (org-entry-get (point) "TODO"))
           (todo-int (if todo (org-todo-to-int todo) todo-max))
           (priority (org-entry-get (point) "PRIORITY"))
           (priority-int (if priority (string-to-char priority) org-default-priority)))
      (format "%03d %03d" todo-int priority-int)))

  (defun org-sort-entries-by-todo-status ()
    "Sort Org TODO entries by their status."
    (interactive)
    (org-sort-entries nil ?f #'org-sort-entries--todo-status-key))

  (defun org-archive-done-tasks-in-file ()
    "Archive all tasks marked done."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/DONE" 'file))

  (defface org-emphasis-marker-face '((t (:inherit shadow)))
    "Face for Org emphasis markers"
    :group 'org-faces)

  ;; This is a re-definition of a built in function.
  ;; TODO Follow up with Org mailing list on this approach.
  (defun org-do-emphasis-faces (limit)
    "Run through the buffer and emphasize strings."
    (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\)"
                            (car org-emphasis-regexp-components))))
      (catch :exit
        (while (re-search-forward quick-re limit t)
          (let* ((marker (match-string 2))
                 (verbatim? (member marker '("~" "="))))
            (when (save-excursion
                    (goto-char (match-beginning 0))
                    (and
                     ;; Do not match table hlines.
                     (not (and (equal marker "+")
                               (org-match-line
                                "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
                     ;; Do not match headline stars.  Do not consider
                     ;; stars of a headline as closing marker for bold
                     ;; markup either.
                     (not (and (equal marker "*")
                               (save-excursion
                                 (forward-char)
                                 (skip-chars-backward "*")
                                 (looking-at-p org-outline-regexp-bol))))
                     ;; Match full emphasis markup regexp.
                     (looking-at (if verbatim? org-verbatim-re org-emph-re))
                     ;; Do not span over paragraph boundaries.
                     (not (string-match-p org-element-paragraph-separate
                                          (match-string 2)))
                     ;; Do not span over cells in table rows.
                     (not (and (save-match-data (org-match-line "[ \t]*|"))
                               (string-match-p "|" (match-string 4))))))
              (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist)))
                (font-lock-prepend-text-property
                 (match-beginning 2) (match-end 2) 'face face)
                (when verbatim?
                  (org-remove-flyspell-overlays-in
                   (match-beginning 0) (match-end 0))
                  (remove-text-properties (match-beginning 2) (match-end 2)
                                          '(display t invisible t intangible t)))
                (add-text-properties (match-beginning 2) (match-end 2)
                                     '(font-lock-multiline t org-emphasis t)))

              (font-lock-prepend-text-property
               (match-beginning 3) (match-end 3) 'face 'org-emphasis-marker-face)
              (font-lock-prepend-text-property
               (match-end 4) (match-beginning 5) 'face 'org-emphasis-marker-face)

              (when org-hide-emphasis-markers
                (add-text-properties (match-end 4) (match-beginning 5)
                                     '(invisible org-link))
                (add-text-properties (match-beginning 3) (match-end 3)
                                     '(invisible org-link))))
            (throw :exit t))))))

  (use-package org-download
    :after org
    :hook
    (dired-mode-hook . org-download-enable))

  ;; Required for Org html export
  ;; (use-package htmlize
  ;;   :commands
  ;;   htmlize-file
  ;;   htmlize-region
  ;;   htmlize-buffer
  ;;   htmlize-many-files
  ;;   htmlize-many-files-dired
  ;;   org-html-htmlize-generate-css)

  (use-package org-preview-html
    :commands
    org-preview-html-mode)

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-switchb)
   :map m-map
   ("s" . org-search-org-directory)
   ("n" . (lambda () (interactive)
            (find-file (expand-file-name "new-note.org"))))
   ("o" . (lambda () (interactive) (find-file org-directory)))
   :map org-mode-map
   ("C-M-}" . org-forward-sentence)
   ("C-M-{" . org-backward-sentence)
   ("M-S-<up>" . org-move-subtree-up)
   ("M-S-<down>" . org-move-subtree-down)
   ("s->" . org-shiftright)
   ("s-<" . org-shiftleft)
   :map visual-line-mode-map
   ;; Don't shadow mwim and org-mode bindings
   ([remap move-beginning-of-line] . nil)))

(use-package poporg
  :bind
  ("C-c C-'" . poporg-dwim))

;; Requires Org 9.3, which I'm not using yet.
;; (use-package orglink
;;   :after org
;;   :hook
;;   (prog-mode-hook . orglink-mode))

;;;; Calendar and Journal

(use-package calendar
  :commands
  calendar-current-date
  :config
  (defun calendar-iso8601-date-string (date)
    "Create an ISO8601 date string from DATE."
    (cl-destructuring-bind (month day year) date
      (concat (format "%4i" year)
              "-"
              (format "%02i" month)
              "-"
              (format "%02i" day))))

  (defun calendar-date-add-days (date days)
    "Add DAYS to DATE."
    (calendar-gregorian-from-absolute (+ (calendar-absolute-from-gregorian date) days)))

  (defun calendar-choose-date ()
    "Interactively choose DATE and return it as an ISO 8601 string."
    (let* ((today (calendar-current-date))
           (day-offsets '(0 -1 -2 -3 -4 -5 -6 -7))
           (dates (mapcar (apply-partially #'calendar-date-add-days today) day-offsets))
           (date-strings (mapcar #'calendar-iso8601-date-string dates)))
      (completing-read "Date: " date-strings nil nil (substring (car date-strings) 0 7))))

  (defun calendar-insert-date (date)
    "Interactively choose a DATE in ISO 8601 format and insert it at point."
    (interactive (list (calendar-choose-date)))
    (insert date))

  (defun calendar-insert-date-today ()
    "Insert today's date in ISO 8601 format."
    (interactive)
    (insert (calendar-iso8601-date-string (calendar-current-date))))

  (defvar journal-directory)

  (defun journal-new-entry ()
    "Create a new journal entry."
    (interactive)
    (let ((date (calendar-choose-date)))
      (find-file (expand-file-name (concat date ".md") journal-directory))
      (if (= 0 (buffer-size))
          (progn
            (insert "journal")
            (yas-expand)))))

  :commands
  (calendar-gregorian-from-absolute
   new-journal-entry
   calendar-insert-date
   calendar-choose-date)
  :bind
  ("M-m i d" . calendar-insert-date)
  ("M-m i t" . calendar-insert-date-today)
  ("M-m j" . journal-new-entry))

;;;; Reading

(defun brew-prefix (package)
  "Get the `homebrew' install prefix for PACKAGE."
  (shell-command-to-string (format "printf %%s \"$(brew --prefix %s)\"" package)))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; Might need to download, make, and install poppler from source.

  ;; (let ((orig (getenv "PKG_CONFIG_PATH")))
  ;;   (setenv "PKG_CONFIG_PATH"
  ;;           (concat "" orig
  ;;                   ":" (brew-prefix "poppler") "/lib/pkgconfig"
  ;;                   ":" (brew-prefix "libffi") "/lib/pkgconfig"
  ;;                   ":" (brew-prefix "glib") "/lib/pkgconfig"
  ;;                   ":" (brew-prefix "pcre") "/lib/pkgconfig"
  ;;                   ":" (brew-prefix "libpng") "/lib/pkgconfig"))
  ;;   (pdf-loader-install)
  ;;   (setenv "PKG_CONFIG_PATH" orig))
  (pdf-loader-install)
  :bind
  (:map pdf-view-mode-map
        ("s-f" . isearch-forward)))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))


;;;; Navigation

;; Navigation tools

(defun fullscreen ()
  "Toggle fullscreen mode."
  (interactive)
  (set-frame-parameter
   nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun scroll-handle-hscroll ()
  "Ensure point stays on the proper column when scrolling.

Ripped out of function `line-move-visual'."
  (let ((hscroll (window-hscroll)))
    (if (and (consp temporary-goal-column)
             (memq last-command `(next-line previous-line scroll-window-up
                                            scroll-window-down ,this-command)))

        (progn
          (line-move-to-column (truncate (car temporary-goal-column)))
          (message "moved col to %s" (car temporary-goal-column))
          ;; If so, there's no need to reset `temporary-goal-column',
          ;; but we may need to hscroll.
          (when (or (/= (cdr temporary-goal-column) hscroll)
                    (>  (cdr temporary-goal-column) 0))
            (set-window-hscroll (selected-window) (cdr temporary-goal-column))))

      ;; Otherwise, we should reset `temporary-goal-column'.
      (let ((posn (posn-at-point))
            x-pos)
        (cond
         ;; Handle the `overflow-newline-into-fringe' case
         ;; (left-fringe is for the R2L case):
         ((memq (nth 1 posn) '(right-fringe left-fringe))
          (setq temporary-goal-column (cons (window-width) hscroll)))
         ((car (posn-x-y posn))
          (setq x-pos (- (car (posn-x-y posn)) (line-number-display-width t)))
          ;; In R2L lines, the X pixel coordinate is measured from the
          ;; left edge of the window, but columns are still counted
          ;; from the logical-order beginning of the line, i.e. from
          ;; the right edge in this case.  We need to adjust for that.
          (if (eq (current-bidi-paragraph-direction) 'right-to-left)
              (setq x-pos (- (window-body-width nil t) 1 x-pos)))
          (setq temporary-goal-column
                (cons (/ (float x-pos)
                         (frame-char-width))
                      hscroll)))
         (executing-kbd-macro
          ;; When we move beyond the first/last character visible in
          ;; the window, posn-at-point will return nil, so we need to
          ;; approximate the goal column as below.
          (setq temporary-goal-column
                (mod (current-column) (window-text-width)))))))))

(defun scroll-window-up ()
  "Scroll the buffer up, keeping point in place relative to the window."
  (interactive)
  (scroll-down-command 1)
  (scroll-handle-hscroll))

(defun scroll-window-down ()
  "Scroll the buffer up, keeping point in place relative to the window."
  (interactive)
  (scroll-up-command 1)
  (scroll-handle-hscroll))

(defun scroll-up-margin ()
  "Move point to the top of the window.

If it's already there, scroll `scroll-margin' lines up."
  (interactive)
  (let ((line (line-number-at-pos))
        (line-beg (line-number-at-pos (window-start))))
    (if (= (- line line-beg) scroll-margin)
        (forward-line (- scroll-margin))
      (forward-line (+ (- line-beg line) scroll-margin)))))

(defun scroll-down-margin ()
  "Move point to the bottom of the window.

If it's already there, scroll `scroll-margin' lines down."
  (interactive)
  (let ((line (line-number-at-pos))
        (line-end (- (line-number-at-pos (window-end)) 2)))
    (if (= (- line-end line) scroll-margin)
        (forward-line scroll-margin)
      (forward-line (- line-end line scroll-margin)))))

(defvar scratch-other-modes
  '(lisp-interaction-mode js-mode js-jsx-mode)
  "Modes to add to the new scratch buffer list.
This list exists because these modes may not be added
  automatically. See `list-major-modes'.")

(defun filter-buffers-by-name (regexp)
  "Return a list of buffers whose names match REGEXP."
  (seq-filter (lambda (b) (string-match-p regexp (buffer-name b)))
              (buffer-list)))

(defun filter-buffers-by-mode (mode)
  "Return a list of buffers whose major mode is MODE."
  (when (stringp mode) (setq mode (intern mode)))
  (seq-filter (lambda (b) (eq (buffer-local-value 'major-mode b) mode))
              (buffer-list)))

(defun list-buffer-major-modes ()
  "Return a list of all major modes currently in use in open buffers."
  (delete-dups (mapcar (lambda (b) (buffer-local-value 'major-mode b))
                       (buffer-list))))

(defun list-major-modes ()
  "Return a list of all major modes.

It actually does not list them all because I don't know how to do
  that. So, we find only ones which are associated with a magic
  string or file extension."
  (delete-dups (mapcar #'cdr (append magic-mode-alist
                                     auto-mode-alist
                                     magic-fallback-mode-alist))))

(defun scratch-new-buffer (arg)
  "Create or go to a scratch buffer.

If ARG is provided then create a new buffer regardless of whether
one exists already."
  (interactive "P")
  (let* ((default-directory "/tmp")
         (uniquify-buffer-name-style nil)
         (buffer (if arg
                     (generate-new-buffer "*scratch*")
                   (get-buffer-create "*scratch*")))
         (win (get-buffer-window buffer)))
    (if win
        (select-window win)
      (progn (switch-to-buffer buffer)
             (funcall-interactively initial-major-mode)))))

(defun scratch-new-buffer-other-window (arg)
  "Create or go to a scratch buffer in the current mode.

If ARG is provided then create a new buffer regardless of whether
one exists already.

See `scratch-new-buffer'."
  (interactive "P")
  (switch-to-buffer-other-window (current-buffer))
  (scratch-new-buffer arg))

(defun switch-to-buffer-by-mode (mode)
  "Interactively choose a major MODE, then choose a buffer of that mode."
  (interactive
   (list (ivy-read "Choose buffers for major mode: "
                   (list-buffer-major-modes)
                   :history 'switch-to-buffer-by-mode-history
                   :action 'switch-to-buffer-by-mode)))
  (when (stringp mode) (setq mode (intern mode)))
  (let ((buffers (mapcar #'buffer-name (filter-buffers-by-mode mode))))
    (ivy-read (format "%s buffers: " mode) buffers
              :keymap ivy-switch-buffer-map
              :action #'ivy--switch-buffer-action
              :matcher #'ivy--switch-buffer-matcher
              :preselect (when (eq major-mode mode) (cadr buffers))
              ;; Use the `ivy-switch-buffer' actions.
              :caller #'ivy-switch-buffer)))

;; kill buffer and window
(defun kill-other-buffer-and-window ()
  "Kill the buffer in the other window."
  (interactive)
  (select-window (next-window))
  (kill-buffer-and-window))

;; https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  "Toggle windows between horizontal and vertical split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun find-file-at-point-with-line (&optional filename)
  "Open FILENAME at point and move point to line specified next to file name."
  (interactive)
  (require 'ffap)
  (eval-when-compile
    (defvar ffap-url-fetcher)
    (defvar ffap-pass-wildcards-to-dired)
    (defvar ffap-dired-wildcards)
    (defvar ffap-directory-finder)
    (defvar ffap-file-finder)
    (defvar ffap-newfile-prompt))
  (let* ((filename (or filename (if current-prefix-arg (ffap-prompter) (ffap-guesser))))
         (line-number
          (and (or (looking-at ".* line \\(\[0-9\]+\\)")
                   (looking-at "[^:]*:\\(\[0-9\]+\\)"))
               (string-to-number (match-string-no-properties 1))))
         (column-number
          (or
           (and (looking-at "[^:]*:\[0-9\]+:\\(\[0-9\]+\\)")
                (string-to-number (match-string-no-properties 1)))
           0)))
    (message "%s --> %s:%s" filename line-number column-number)
    (cond ((ffap-url-p filename)
           (let (current-prefix-arg)
             (funcall ffap-url-fetcher filename)))
          ((and line-number
                (file-exists-p filename))
           (progn (find-file-other-window filename)
                  (goto-char (point-min))
                  (forward-line (1- line-number))
                  (forward-char column-number)))
          ((and ffap-pass-wildcards-to-dired
                ffap-dired-wildcards
                (string-match ffap-dired-wildcards filename))
           (funcall ffap-directory-finder filename))
          ((and ffap-dired-wildcards
                (string-match ffap-dired-wildcards filename)
                find-file-wildcards
                ;; Check if it's find-file that supports wildcards arg
                (memq ffap-file-finder '(find-file find-alternate-file)))
           (funcall ffap-file-finder (expand-file-name filename) t))
          ((or (not ffap-newfile-prompt)
               (file-exists-p filename)
               (y-or-n-p "File does not exist, create buffer? "))
           (funcall ffap-file-finder
                    ;; expand-file-name fixes "~/~/.emacs" bug sent by CHUCKR.
                    (expand-file-name filename)))
          ;; User does not want to find a non-existent file:
          ((signal 'file-error (list "Opening file buffer"
                                     "no such file or directory"
                                     filename))))))

(defun parse-colon-notation (filename)
  "Parse FILENAME in the format expected by `server-visit-files'.

Modify it so that `filename:line:column' is is reformatted the
way Emacs expects."
  (let ((name (car filename)))
    (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
        (cons
         (match-string 1 name)
         (cons (string-to-number (match-string 2 name))
               (string-to-number (or (match-string 3 name) ""))))
      filename)))

(defun wrap-colon-notation (f &rest args)
  "Wrap F (`server-visit-files') and modify ARGS to support colon notation.

Open files with emacsclient with cursors according to colon
notation. When the file name has line numbers and optionally
columns specified like `filename:line:column', parse those and
return them in the Emacs format."
  (message "%s" args)
  (apply f (cons (mapcar #'parse-colon-notation (car args)) (cdr args))))

;; Make `emacsclient' support line:column notation.
(advice-add 'server-visit-files :around #'wrap-colon-notation)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for line number N."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (forward-line)
        (let ((n (1- (read-number "Go to line: "))))
          (goto-char (point-min))
          (forward-line n)))
    (linum-mode -1)))

;; Show line in the original buffer from occur mode
(setq list-matching-lines-jump-to-current-line t)

(use-package goto-addr
  :hook
  ((prog-mode-hook text-mode-hook) . goto-address-mode))

(use-package bug-reference
  :custom
  (bug-reference-bug-regexp
   "\\([Bb]ug ?#?\\|[Pp]atch ?#\\|RFE ?#\\|PR [a-z+-]+/\\|SER\\|REQ\\|[Ii]ssue ?#\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)")
  :config
  (defvar bug-reference-dispatch-alist nil
    "Alist where CAR is a regexp to match the type and CADR is a
    URL format string.

Example\:
'((\"SER\" \"https://example.service-now.com/nav_to.do?uri=u_task_service_request.do?sys_id=SER%s\"))")

  ;; Idea stolen from https://github.com/arnested/bug-reference-github
  (defun bug-reference-dispatch-url-github-or-gitlab (_type ref)
    "With Bug TYPE and REF, return a complete URL."
    (when (vc-git-root (or (buffer-file-name) default-directory))
      (let ((remote (shell-command-to-string "git ls-remote --get-url")))
        (when (string-match "\\(git\\(?:hu\\|la\\)b.com\\)[/:]\\(.+?\\)\\(\\.git\\)?$"
                            remote)
          (format "https://%s/%s/issues/%s"
                  (match-string-no-properties 1 remote)
                  (match-string-no-properties 2 remote)
                  ref)))))

  (defun bug-reference-dispatch-url ()
    "Get the bug reference URL using `match-string' contents."
    (let ((type (match-string-no-properties 1))
          (ref (match-string-no-properties 2)))
      (or (cl-some (lambda (entry)
                     (cond
                      ((and (stringp (car entry)) (string-match-p (car entry) type))
                       (format (cadr entry) ref))))
                   bug-reference-dispatch-alist)
          (bug-reference-dispatch-url-github-or-gitlab type ref))))

  (setq bug-reference-url-format #'bug-reference-dispatch-url)
  :hook
  (prog-mode-hook . bug-reference-prog-mode)
  ((org-mode-hook text-mode-hook) . bug-reference-mode))

(use-package winner
  :defer 5
  :commands
  winner-mode
  :config
  (defun winner-wrong-window ()
    "Open the last opened buffer in the other window."
    (interactive)
    (let* ((current (window-list))
           (previous (save-window-excursion (winner-undo) (window-list)))
           (window (seq-some (lambda (w) (not (memq w previous))) current))
           (buffer (window-buffer window)))
      (winner-undo)
      (other-window 1)
      (switch-to-buffer buffer)))

  (winner-mode)
  :bind
  ("C-c [" . winner-undo)
  ("s-[" . winner-undo)
  ("C-c ]" . winner-redo)
  ("s-]" . winner-redo)
  ("C-c z" . winner-wrong-window))

(use-package buffer-move
  :bind
  ("C-H-W" . buf-move-up)
  ("C-H-S" . buf-move-down)
  ("C-H-A" . buf-move-left)
  ("C-H-D" . buf-move-right))

(use-package winum
  :defer 3
  :custom
  (winum-auto-setup-mode-line nil)
  :commands
  winum-mode
  :config
  (winum-mode)
  :bind
  ("s-1" . winum-select-window-1)
  ("C-c 1" . winum-select-window-1)
  ("s-2" . winum-select-window-2)
  ("C-c 2" . winum-select-window-2)
  ("s-3" . winum-select-window-3)
  ("C-c 3" . winum-select-window-3)
  ("s-4" . winum-select-window-4)
  ("C-c 4" . winum-select-window-4)
  ("s-5" . winum-select-window-5)
  ("C-c 5" . winum-select-window-5)
  ("s-6" . winum-select-window-6)
  ("C-c 6" . winum-select-window-6)
  ("s-7" . winum-select-window-7)
  ("C-c 7" . winum-select-window-7)
  ("s-8" . winum-select-window-8)
  ("C-c 8" . winum-select-window-8)
  ("s-9" . winum-select-window-9)
  ("C-c 9" . winum-select-window-9)
  ("s-0" . winum-select-window-0)
  ("C-c 0" . winum-select-window-0))

(use-package eyebrowse
  :demand t
  :custom
  (eyebrowse-new-workspace t)
  (eyebrowse-mode-line-separator " ")
  :config
  (defvar eyebrowse-last-window-config nil
    "Variable used to save and restore `eyebrowse' window
configuration. Persistence is handled by `psession'.")

  (defun eyebrowse-restore-window-config ()
    "Restore eyebrowse window config to variable.
This is for restoration from disk by `psession'."
    (when (bound-and-true-p eyebrowse-last-window-config)
      (eyebrowse--set 'window-configs eyebrowse-last-window-config)))

  (defun eyebrowse-save-window-config ()
    "Save eyebrowse window config to variable.
This is for serialization to disk by `psession'."
    (setq eyebrowse-last-window-config (eyebrowse--get 'window-configs)))

  (defun eyebrowse-activate ()
    "Enable `eyebrowse-mode' and restore the last window-config."
    (eyebrowse-mode)
    (eyebrowse-restore-window-config))

  :hook
  ((psession-autosave-mode-hook kill-emacs-hook) . eyebrowse-save-window-config)
  (emacs-startup-hook . eyebrowse-activate)
  :bind
  ("H-1" . eyebrowse-switch-to-window-config-1)
  ("C-c C-1" . eyebrowse-switch-to-window-config-1)
  ("H-2" . eyebrowse-switch-to-window-config-2)
  ("C-c C-2" . eyebrowse-switch-to-window-config-2)
  ("H-3" . eyebrowse-switch-to-window-config-3)
  ("C-c C-3" . eyebrowse-switch-to-window-config-3)
  ("H-4" . eyebrowse-switch-to-window-config-4)
  ("C-c C-4" . eyebrowse-switch-to-window-config-4)
  ("H-5" . eyebrowse-switch-to-window-config-5)
  ("C-c C-5" . eyebrowse-switch-to-window-config-5)
  ("H-6" . eyebrowse-switch-to-window-config-6)
  ("C-c C-6" . eyebrowse-switch-to-window-config-6)
  ("H-7" . eyebrowse-switch-to-window-config-7)
  ("C-c C-7" . eyebrowse-switch-to-window-config-7)
  ("H-8" . eyebrowse-switch-to-window-config-8)
  ("C-c C-8" . eyebrowse-switch-to-window-config-8)
  ("H-9" . eyebrowse-switch-to-window-config-9)
  ("C-c C-9" . eyebrowse-switch-to-window-config-9)
  ("H-0" . eyebrowse-switch-to-window-config-0)
  ("C-c C-0" . eyebrowse-switch-to-window-config-0))

;; Create friendly names for buffers with the same name
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; scratch
(setq initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

;; Try to re-use help buffers of different sorts
(setq display-buffer-alist
      `((,(rx bos
              (or "*Apropos*" "*eww*" "*Help*" "*helpful" "*info*" "*Summary*")
              (0+ not-newline))
         (display-buffer-reuse-mode-window display-buffer-pop-up-window)
         (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))

(with-eval-after-load 'ediff-wind
  (defvar ediff-window-setup-function)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package outorg
  :defer 4
  :init
  (defvar outline-minor-mode-prefix "\M-#"))

(use-package outshine
  :defer 4
  :after outline outorg
  :config
  ;; (put 'narrow-to-region 'disabled t)

  (defun outline-show-current-sublevel ()
    "Show only the current top level section."
    (interactive)
    (unless (bound-and-true-p 'outline-minor-mode)
      (outline-minor-mode t))
    (outline-hide-sublevels 1)
    (outline-show-subtree))

  (defun outline-subtree-previous ()
    "Go to and expand previous sublevel."
    (interactive)
    (unless (bound-and-true-p outline-minor-mode)
      (outline-minor-mode t))
    (outline-previous-visible-heading 1)
    (outline-show-subtree))

  (defun outline-subtree-next ()
    "Go to and expand previous sublevel."
    (interactive)
    (unless (bound-and-true-p outline-minor-mode)
      (outline-minor-mode t))
    (outline-next-visible-heading 1)
    (outline-show-subtree))

  (defun outshine-narrow-dwim (&rest _args)
    (unless (outline-on-heading-p t)
      (outline-previous-visible-heading 1)))

  ;; Narrowing now works within the headline rather than requiring to be on it
  (advice-add 'outshine-narrow-to-subtree :before #'outshine-narrow-dwim)

  (defhydra hydra-outline (:color pink :hint nil)
    "
Outline

^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_s_ sublevels     _a_ all         _u_ up
_t_ body          _e_ entry       _n_ next visible
_o_ other         _i_ children    _p_ previous visible
_c_ entry         _k_ branches    _f_ forward same level
_l_ leaves        _s_ subtree     _b_ backward same level
_d_ subtree

"
    ;; Hide
    ("q" outline-hide-sublevels) ; Hide everything but the top-level headings
    ("t" outline-hide-body)      ; Hide everything but headings (all body lines)
    ("o" outline-hide-other)     ; Hide other branches
    ("c" outline-hide-entry)     ; Hide this entry's body
    ("l" outline-hide-leaves)    ; Hide body lines in this entry and sub-entries
    ("d" outline-hide-subtree)   ; Hide everything in this entry and sub-entries
    ;; Show
    ("a" outline-show-all)      ; Show (expand) everything
    ("e" outline-show-entry)    ; Show this heading's body
    ("i" outline-show-children) ; Show this heading's immediate child sub-headings
    ("k" outline-show-branches) ; Show all sub-headings under this heading
    ("s" outline-show-subtree) ; Show (expand) everything in this heading & below
    ;; Move
    ("u" outline-up-heading)               ; Up
    ("n" outline-next-visible-heading)     ; Next
    ("p" outline-previous-visible-heading) ; Previous
    ("f" outline-forward-same-level)       ; Forward - same level
    ("b" outline-backward-same-level)      ; Backward - same level
    ("q" nil "leave"))

  :hook
  (outline-minor-mode-hook . outshine-mode)
  (prog-mode-hook . outline-minor-mode)
  :bind
  (:map outline-minor-mode-map
        ("C-c #" . hydra-outline/body)
        ;; Don't shadow smarparens or org bindings
        ("M-<up>" . nil)
        ("M-<down>" . nil)
        ("<backtab>" . outshine-cycle-buffer)
        ("M-=" . outline-show-current-sublevel)
        ("M-p" . outline-subtree-previous)
        ("M-n" . outline-subtree-next)))

;; hs-minor-mode for folding top level forms
(use-package hideshow
  :defer 4
  :custom
  (hs-hide-comments-when-hiding-all nil)
  :commands
  hs-show-all
  hs-hide-all
  hs-show-block
  hs-hide-block
  hs-hide-level
  :config
  (defhydra hydra-hs (:color pink :hint nil)
    "
Hideshow

Hide^^            ^Show^            ^Toggle^    ^Navigation^
----------------------------------------------------------------
_h_ hide all      _s_ show all      _t_ toggle    _n_ next line
_d_ hide block    _a_ show block                _p_ previous line
_l_ hide level

_q_ quit
"
    ("s" hs-show-all)
    ("h" hs-hide-all)
    ("a" hs-show-block)
    ("d" hs-hide-block)
    ("t" hs-toggle-hiding)
    ("l" hs-hide-level)
    ("n" forward-line)
    ("p" (forward-line -1))
    ("q" nil))
  :bind
  (:map hs-minor-mode-map
        ("C-c @" . hydra-hs/body)
        ("C-<tab>" . hs-toggle-hiding)))

(use-package symbol-overlay
  :hook
  (prog-mode-hook . symbol-overlay-mode)
  :bind
  ("C-s-n" . symbol-overlay-jump-next)
  ("C-s-p" . symbol-overlay-jump-prev)
  ("C-s-r" . symbol-overlay-rename)
  ("C-s-5" . symbol-overlay-query-replace))

(use-package iy-go-to-char
  :custom
  (iy-go-to-char-forward ?f)
  (iy-go-to-char-backward ?b)
  :config
  (with-eval-after-load 'multiple-cursors
    (defvar mc/cursor-specific-vars)
    (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos))
  :bind
  (("M-F" . iy-go-up-to-char)
   ("M-B" . iy-go-up-to-char-backward)
   ("C-M-\"" . iy-go-to-or-up-to-continue)
   ("C-M-:" . iy-go-to-or-up-to-continue-backward)))

;; TODO: Fix highlighting of things like #12345
(use-package rainbow-mode
  :hook
  ((css-mode-hook emacs-lisp-mode-hook sass-mode-hook) . rainbow-mode))

(use-package crux
  :bind
  (("C-c C-j" . crux-eval-and-replace)
   ("M-s-<backspace>" . crux-kill-line-backwards)
   ("C-s-c" . crux-duplicate-current-line-or-region)
   ("C-c C-o" . crux-open-with)
   :map m-window-map
   ("k" . crux-kill-other-buffers)
   :map m-file-map
   ("d" . crux-delete-file-and-buffer)
   ("r" . crux-rename-file-and-buffer)))

(defun save-kill-buffers-and-quit ()
  "Kill all buffers, clean up tramp caches, and quit Emacs."
  (interactive)
  (save-some-buffers)
  (desktop-clear)
  (tramp-cleanup-all)
  (kill-emacs))

(defmacro specialize-beginning-of-buffer (mode &rest forms)
  "Define a special version of `beginning-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-min' and then FORMS are evaluated. If the point did not
change because of the evaluation of FORMS, jump unconditionally
to `point-min'. This way repeated invocations toggle between real
beginning and logical beginning of the buffer.

https://fuco1.github.io/2017-05-06-Enhanced-beginning--and-end-of-buffer-in-special-mode-buffers-(dired-etc.).html"
  (declare (indent 1))
  (let ((fname (intern (concat (symbol-name mode) "-beginning-of-buffer")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-min))
           ,@forms
           (when (= p (point))
             (goto-char (point-min)))))
       (add-hook ',mode-hook
                 (lambda ()
                   (define-key ,mode-map
                     [remap beginning-of-buffer] ',fname))))))

(defmacro specialize-end-of-buffer (mode &rest forms)
  "Define a special version of `end-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-max' and then FORMS are evaluated.  If the point did
not change because of the evaluation of FORMS, jump
unconditionally to `point-max'.  This way repeated invocations
toggle between real end and logical end of the buffer.

https://fuco1.github.io/2017-05-06-Enhanced-beginning--and-end-of-buffer-in-special-mode-buffers-(dired-etc.).html"
  (declare (indent 1))
  (let ((fname (intern (concat "my-" (symbol-name mode) "-end-of-buffer")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-max))
           ,@forms
           (when (= p (point))
             (goto-char (point-max)))))
       (add-hook ',mode-hook
                 (lambda ()
                   (define-key ,mode-map
                     [remap end-of-buffer] ',fname))))))

(specialize-beginning-of-buffer dired
  (while (not (ignore-errors (dired-get-filename))) (dired-next-line 1)))
(specialize-end-of-buffer dired (dired-previous-line 1))

(specialize-beginning-of-buffer occur (occur-next 1))
(specialize-end-of-buffer occur (occur-prev 1))

(specialize-beginning-of-buffer ivy-occur-grep (ivy-occur-next-line 1))
(specialize-end-of-buffer ivy-occur-grep (ivy-occur-previous-line 1))

(defvar ibuffer-mode-map)
(specialize-beginning-of-buffer ibuffer (ibuffer-forward-line 1))
(specialize-end-of-buffer ibuffer (ibuffer-backward-line 1))

(defvar vc-dir-mode-map)
(specialize-beginning-of-buffer vc-dir (vc-dir-next-line 1))
(specialize-end-of-buffer vc-dir (vc-dir-previous-line 1))

(specialize-beginning-of-buffer recentf-dialog
  (when (re-search-forward "^  \\[" nil t) (goto-char (match-beginning 0))))
(specialize-end-of-buffer recentf-dialog (re-search-backward "^  \\[" nil t))

(declare-function org-agenda-next-item 'org-agenda)
(declare-function org-agenda-previous-item 'org-agenda)
(defvar org-agenda-mode-map)
(specialize-beginning-of-buffer org-agenda (org-agenda-next-item 1))
(specialize-end-of-buffer org-agenda (org-agenda-previous-item 1))

(defvar rg-mode-map)
(specialize-beginning-of-buffer rg (compilation-next-error 1))
(specialize-end-of-buffer rg (compilation-previous-error 1))

(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
  ^Mark^         ^Actions^         ^View^          ^Select^              ^Navigation^
  _m_ mark      _D_ delete       _g_ refresh    _q_ quit             _k_   ↑    _h_
  _u_ unmark    _s_ save marked  _S_ sort       _TAB_ toggle         _RET_ visit
  _*_ specific  _a_ all actions  _/_ filter     _o_ other window     _j_   ↓    _l_
  _t_ toggle    _._ toggle hydra _H_ help       C-o other win no-select
  "
  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)
  ("t" ibuffer-toggle-marks)

  ("D" ibuffer-do-delete)
  ("s" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("S" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)
  ("H" describe-mode :color blue)

  ("h" ibuffer-backward-filter-group)
  ("k" ibuffer-backward-line)
  ("l" ibuffer-forward-filter-group)
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)

  ("TAB" ibuffer-toggle-filter-group)

  ("o" ibuffer-visit-buffer-other-window :color blue)
  ("q" quit-window :color blue)
  ("." nil :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                                     :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                       :after-exit
                                       (if (eq major-mode 'ibuffer-mode)
                                           (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

;; (use-package matcha
;;   :defer 7
;;   :git "https://github.com/jojojames/matcha.git"
;;   :custom
;;   (matcha-mode-list
;;    '(cider dired js json-mode lua-mode org
;;            (:file projectile :autoloads matcha-projectile)
;;            python restclient smerge-mode term vc-dir vc-git web-mode))
;;   :config
;;   (matcha-setup))

(use-package ibuffer
  :bind
  (:map ibuffer-mode-map
        ("." . hydra-ibuffer-main/body)))

(defvar hide-mode-line nil
  "Save old `mode-line-format'.")

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(defun window-config-dotemacs ()
  "Set up dotemacs window config."
  (interactive)
  (delete-other-windows)
  (dired "~/.emacs.d/init")
  (switch-to-buffer-other-window (current-buffer))
  (find-file "~/.emacs.d/TODO.org")
  (other-window 1))

(defun window-config-org ()
  "Set up Org window config."
  (interactive)
  (delete-other-windows)
  (dired org-directory)
  (switch-to-buffer-other-window (current-buffer))
  (find-file (expand-file-name "TODO.org" org-directory))
  (other-window 1))

(bind-keys
 ("M-s-q" . save-kill-buffers-and-quit)
 ("s-x" . kill-line-or-region)
 ("s-c" . copy-line-or-region)
 ("s-v" . clipboard-yank-and-indent)
 ("s-/" . comment-toggle)
 ("C-c i" . os-reveal-file)
 ("s-n" . scratch-new-buffer)
 ("s-N" . scratch-new-buffer-other-window)
 ("C-c C-n"f . scratch-new-buffer)
 ("C-c M-n" . scratch-new-buffer-other-window)
 ("C-S-p" . scroll-up-margin)
 ("C-S-n" . scroll-down-margin)
 ("H-p" . scroll-window-up)
 ("H-n" . scroll-window-down)

 ;; Full screen
 ("C-s-f" . fullscreen)

 ;; Quick switch buffers
 ("C-x C-b" . ibuffer)
 ("s-}" . next-buffer)
 ("C-c }" . next-buffer)
 ("s-{" . previous-buffer)
 ("C-c {" . previous-buffer)
 ("C-s-j" . switch-to-buffer-by-mode)
 ("C-c M-j" . switch-to-buffer-by-mode)

 ;; windmove
 ("H-a" . windmove-left)
 ("H-d" . windmove-right)
 ("H-w" . windmove-up)
 ("H-s" . windmove-down)
 ("M-]" . windmove-right)
 ("M-[" . windmove-left)

 ;; Resize windows
 ("M-s-<up>" . shrink-window)
 ("M-s-<down>" . enlarge-window)
 ("M-s-<left>" . shrink-window-horizontally)
 ("M-s-<right>" . enlarge-window-horizontally)

 ;; Kill buffer and window at the same time.
 ("M-s-w" . kill-buffer-and-window)
 ("M-s-W" . kill-other-buffer-and-window)

 ;; Navigate with mark
 ("M-s-," . pop-to-mark-command)
 ("C-c ," . pop-to-mark-command)
 ("s-," . pop-global-mark)
 ("C-c C-," . pop-global-mark)

 ;; Xref
 ("s-R" . xref-find-definitions-other-window)

 ("C-c C-f" . find-file-at-point-with-line)

 :map ctl-x-4-map
 ("t" . toggle-window-split)

 :map m-window-map
 ("o" . window-config-org)
 ("e" . window-config-dotemacs)

 :map m-toggle-map
 ("e" . toggle-debug-on-error)
 ("q" . toggle-debug-on-quit)
 ("f" . auto-fill-mode)
 ("l" . toggle-truncate-lines)
 ("m" . hidden-mode-line-mode)
 ("w" . whitespace-mode))


;;;; Search

;; Search and Project Management

(defun replace-regexp-entire-buffer-immediately (pattern replacement)
  "Immediately replace PATTERN with REPLACEMENT throughout the buffer."
  (interactive
   (let ((args (query-replace-read-args "Replace in entire buffer" t)))
     (setcdr (cdr args) nil)    ; remove third value returned from query---args
     args))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match replacement))))

;; Show line in the original buffer from occur mode
(setq list-matching-lines-jump-to-current-line t)

(use-package re-builder
  :defer 5
  :custom
  ;; string syntax means you don't need to double escape things.
  (reb-re-syntax 'string)
  :config
  (use-package pcre2el
    :hook
    ((emacs-lisp-mode-hook lisp-interaction-mode-hook reb-mode-hook) . rxt-mode))
  :bind
  ("C-c r" . re-builder))

(use-package wgrep
  :defer 5
  :custom
  ;; Save changed buffers immediately when exiting wgrep mode
  (wgrep-auto-save-buffer t)
  :bind
  (:map grep-mode-map
        ("C-c C-p" . wgrep-change-to-wgrep-mode)
        :map occur-mode-map
        ("C-c C-p" . wgrep-change-to-wgrep-mode)))

(use-package rg
  :defer 2
  :after
  wgrep-ag
  :ensure-system-package
  (rg . ripgrep)
  :custom
  (rg-keymap-prefix (kbd "C-c M-s"))
  :commands
  rg-enable-default-bindings
  :config
  (rg-enable-default-bindings (kbd "C-r"))
  :hook
  (rg-mode-hook . wgrep-ag-setup))

(use-package ivy
  :defer 0.5
  :custom
  (enable-recursive-minibuffers t)
  (ivy-display-style 'fancy)
  (ivy-count-format "[%d/%d] ")
  ;; Don't exit the minibuffer and pressing backspace on an empty line.
  (ivy-on-del-error-function (lambda (&rest _) nil))
  :commands
  ivy-mode
  ivy-set-index
  ivy-set-actions
  ivy-add-actions
  ivy--reset-state
  :config
  (defun ivy-quit-or-delete-char (arg)
    "Quit Ivy if `C-d' is pressed on empty line, otherwise pass ARG on."
    (interactive "p")
    (if (and (eolp) (looking-back (replace-regexp-in-string "\\[[^]]*\\]" "" ivy--prompt) nil))
        (progn
          (abort-recursive-edit))
      (delete-char arg)))

  (ivy-mode)
  :bind
  (:map ivy-mode-map
        ("C-c C-r" . ivy-resume)
        :map ivy-minibuffer-map
        ("C-e" . ivy-partial-or-done)
        ("C-d" . ivy-quit-or-delete-char)
        ("M-/" . ivy-done)))

(use-package ivy-hydra
  :defer 1
  :after (ivy hydra))

(use-package swiper
  :defer 0.5
  :after ivy
  :config
  (defvar minibuffer-this-command nil
    "Command minibuffer started with.")

  (add-hook 'minibuffer-setup-hook
            (defun minibuffer-set-this-command ()
              (setq minibuffer-this-command real-this-command)))

  (define-key minibuffer-local-map (kbd "C-u") 'minibuffer-restart-with-prefix)
  (define-key ivy-minibuffer-map (kbd "C-u") 'minibuffer-restart-with-prefix)
  (defun minibuffer-restart-with-prefix ()
    "Restart current minibuffer/ivy command with prefix argument.

https://www.reddit.com/r/emacs/comments/cmnumy/weekly_tipstricketc_thread/ew3jyr5?utm_source=share&utm_medium=web2x"
    (interactive)
    (let ((input (ivy--input)))
      (cond ((memq  #'ivy--queue-exhibit post-command-hook)
             (ivy-quit-and-run
               (let ((current-prefix-arg '(4))
                     (ivy-initial-inputs-alist `((,(ivy-state-caller ivy-last) . ,input))))
                 (call-interactively (ivy-state-caller ivy-last)))))
            (t
             (ivy-quit-and-run
               (let ((current-prefix-arg '(4)))
                 (minibuffer-with-setup-hook (lambda ()
                                               (insert input)
                                               (minibuffer-message "C-u"))
                   (call-interactively minibuffer-this-command))))))))
  :bind
  (:map ivy-minibuffer-map
        ("C-c C-c" . ivy-toggle-calling)
        ("s-5" . ivy--replace-regexp-entire-buffer)))

(use-package counsel
  :defer 0.5
  :after ivy
  :custom
  (counsel-find-file-at-point t)
  (counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :commands
  counsel-mode
  :config

  (defun counsel-find-file-edit-path ()
    "Make the path in `counsel-find-file' editable."
    (interactive)
    (let ((dir ivy--directory))
      (setq ivy--old-cands nil)
      (setq ivy--old-re nil)
      (ivy-set-index 0)
      (setq ivy--directory "")
      (setq ivy--all-candidates nil)
      (setq ivy-text "")
      (delete-minibuffer-contents)
      (insert dir)))

  (defun ivy--call-with-current-buffer-in-other-window-action (x)
    "Switch to other window and call command X."
    (switch-to-buffer-other-window (current-buffer)
                                   (call-interactively (intern x))))

  (defun ivy--call-with-other-window-action (x)
    "Switch current buffer to other window and call command X."
    (other-window 1)
    (call-interactively (intern x)))

  (defun counsel-rg-default-directory (f &rest args)
    "Call F (`counsel-rg') with ARGS from `default-directory'.

NOTE: It seems like `counsel-rg' should call itself from
`default-directory' without assistance but in my experience it
looks for a project root directory instead. If we want to search
from the project root, we can use `counsel-projectile-rg', and we
force `counsel-rg' to search in `default-directory.'"
    (let ((initial-input (car args))
          (initial-directory (or (cadr args) default-directory))
          (extra-rg-args (caddr args))
          (rg-prompt (or (cadddr args) (format "(%s) rg: " default-directory))))
      (funcall f initial-input initial-directory extra-rg-args rg-prompt)))

  (advice-add #'counsel-rg :around #'counsel-rg-default-directory)

  (defun reloading (cmd)
    "Wrap CMD, reloading ivy."
    (lambda (x)
      (funcall cmd x)
      (ivy--reset-state ivy-last)))

  (defun given-file (cmd prompt)
    "Wrap in a closure and call CMD, with interactive PROMPT."
    (lambda (source)
      (let ((target
             (let ((enable-recursive-minibuffers t))
               (read-file-name
                (format "%s %s to:" prompt source)))))
        (funcall cmd source target 1))))

  (defun confirm-delete-file (file)
    "Delete FILE with confirmation."
    (dired-delete-file file 'confirm-each-subdirectory))

  (defun counsel-git-grep-other-window-action (x)
    "Switch to other window and call `counsel-git-grep-action'."
    (switch-to-buffer-other-window (current-buffer))
    (counsel-git-grep-action x))

  (ivy-add-actions
   'counsel-M-x
   '(("j" ivy--call-with-current-buffer-in-other-window-action "other window")))
  (ivy-add-actions
   'counsel-find-file
   `(("c" ,(given-file #'copy-file "Copy") "copy")
     ("m" ,(reloading (given-file #'rename-file "Move")) "move")))
  (ivy-add-actions
   'counsel-projectile-find-file
   `(("c" ,(given-file #'copy-file "Copy") "copy")
     ("m" ,(reloading (given-file #'rename-file "Move")) "move")
     ("b" counsel-find-file-cd-bookmark-action "cd bookmark")))
  (ivy-add-actions
   'counsel-switch-buffer
   '(("f" ivy--find-file-action "find file")
     ("j" ivy--switch-buffer-other-window-action "other window")
     ("k" ivy--kill-buffer-action "kill")
     ("r" ivy--rename-buffer-action "rename")))
  (ivy-add-actions
   'counsel-rg
   '(("j" counsel-git-grep-other-window-action "other window")))
  (ivy-set-actions
   'counsel-register
   '(("d" counsel-register-action-delete "delete")
     ("k" counsel-register-action-then-delete "call then delete")))

  (counsel-mode)
  :bind
  (:map counsel-mode-map
        ("M-x" . counsel-M-x)
        ("C-h C-k" . counsel-descbinds)
        ("s-f" . counsel-grep-or-swiper)
        ("s-F" . counsel-rg)
        ("C-x C-f" . counsel-find-file)
        ("C-x f" . counsel-recentf)
        ("C-x j" . counsel-file-jump)
        ("s-B" . counsel-switch-buffer)
        ("C-s-b" . counsel-switch-buffer-other-window)
        ("C-h <tab>" . counsel-info-lookup-symbol)
        ("C-h C-a" . counsel-apropos)
        ("C-c u" . counsel-unicode-char)
        ("C-c g" . counsel-git)
        ("C-c j" . counsel-git-grep)
        ("C-c M-o" . counsel-outline)
        ("M-s-v" . counsel-yank-pop)
        ("M-Y" . counsel-yank-pop)
        ("s-r" . counsel-imenu)
        ;; Don't shadow default binding.
        ([remap yank-pop] . nil)
        ("M-Y" . counsel-yank-pop)
        ([remap find-file] . counsel-find-file))
  (:map ivy-minibuffer-map
        ("M-<backspace>" . counsel-up-directory)
        ("M-DEL" . counsel-up-directory)
        ("C-c C-f" . counsel-find-file-edit-path))
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuffer-history)))

;; Defined in private config.
(defvar code-directory)

(use-package projectile
  :defer 1
  :custom
  (projectile-keymap-prefix (kbd "C-c p"))
  (projectile-completion-system 'ivy)
  (projectile-project-search-path (list code-directory))
  (projectile-globally-ignored-files '("TAGS" "package-lock.json"))
  (projectile-switch-project-action 'projectile-dired)
  (projectile-mode-line nil)
  :commands
  projectile-mode
  projectile-project-root
  :config
  (defun projectile-load-settings (&optional file)
    "Load project elisp settings from FILE.
Look in active project root directory, or if in the case of
  undefined root directory, file is otherwise path resolvable.

https://github.com/jfeltz/projectile-load-settings/blob/master/projectile-load-settings.el"
    (interactive)
    (let ((p (expand-file-name (or file "config.el")
                               (and (fboundp #'projectile-project-root)
                                    (projectile-project-root)))))
      (when (file-exists-p p)
        (load p)
        (message "%s" (concat "Loaded project settings from: " p)))))

  (defun projectile-git-ls-files (&optional dir)
    "List of the tracked files in the git repo, specified by DIR."
    (cd (or dir (projectile-project-root)))
    (cl-remove-if #'string-blank-p
                  (split-string (shell-command-to-string "git ls-files") "\n")))

  (defun projectile-git-ls-files-dired (&optional dir)
    "Dired list of the tracked files in the git repo, specified by DIR."
    (interactive)
    (let ((dir (or dir (projectile-project-root))))
      (dired (cons dir (projectile-git-ls-files dir)))
      (rename-buffer (format "*git ls-files %s*" dir))))

  ;; Why doesn't projectile have this as a default?
  (projectile-register-project-type 'generic nil
                                    :compile ""
                                    :test ""
                                    :test-suffix "_test")
  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm start"
                                    :test "npm test"
                                    :test-suffix ".test")
  (projectile-register-project-type 'clojure-cli '("deps.edn")
                                    :compile "clj "
                                    :test-suffix "_test")
  (projectile-mode)
  :hook
  (projectile-after-switch-project-hook . projectile-load-settings)
  :commands
  projectile-register-project-type
  :bind
  (:map projectile-mode-map
        ("s-}" . projectile-next-project-buffer)
        ("C-c }" . projectile-next-project-buffer)
        ("s-{" . projectile-previous-project-buffer)
        ("C-c {" . projectile-previous-project-buffer)))

(use-package counsel-projectile
  :defer 1
  :after (counsel projectile)
  :custom
  (counsel-projectile-remove-current-buffer t)
  (counsel-projectile-remove-current-project t)
  (compilation-scroll-output t)
  :commands
  counsel-projectile-mode
  :config
  ;; When switching projects, go straight to dired in the project root.
  (setf (car counsel-projectile-switch-project-action) 4)
  (counsel-projectile-mode)
  :bind
  (:map projectile-mode-map
        ("s-p" . counsel-projectile)
        ("s-P" . counsel-projectile-switch-project)
        ("s-r" . counsel-imenu)
        ("M-s-f" . counsel-projectile-rg)
        ("s-b" . counsel-projectile-switch-to-buffer)))

(use-package counsel-etags
  :defer 5
  :after counsel
  :custom
  ;; TODO: Get this working with Clojure (ctags parses namespaces but
  ;; `counsel-etags-find-tag-at-point' doesn't. Wouldn't this be `clojure-mode's
  ;; responsibility? I'm pretty sure it keys off of sexp
  (tags-revert-without-query t)
  ;; Don't warn when TAGS files are large.
  (large-file-warning-threshold nil)
  :hook
  ;; Incrementally update TAGS file when the file is saved.
  (prog-mode-hook
   . (lambda ()
       (add-hook 'after-save-hook
                 'counsel-etags-virtual-update-tags 'append 'local)))
  ((js-mode-hook js2-mode-hook) . counsel-etags-setup-smart-rules)
  :commands
  (counsel-etags-find-tag-at-point counsel-etags-scan-code counsel-etags-list-tag))

(use-package company
  :defer 1
  :custom
  (company-dabbrev-ignore-case t)
  :commands
  global-company-mode
  :config
  (setq company-backends '(company-semantic
                           company-clang
                           company-xcode
                           company-cmake
                           company-capf
                           company-files
                           (company-dabbrev-code company-gtags company-etags company-keywords)
                           company-dabbrev))
  (global-company-mode)
  ;; :hook
  ;; TODO: Figure out how to make company-mode work in the minibuffer.
  ;; (minibuffer-setup . company-mode)
  :bind
  (("M-/" . company-complete)
   :map company-active-map
   ("RET" . nil)
   ("<return>" . nil)
   ("<tab>" . company-complete-selection)
   ("C-s" . company-filter-candidates)
   ("M-?" . company-complete-selection)
   ("M-." . company-show-location)
   :map minibuffer-local-map
   ("M-/" . completion-at-point)
   :map minibuffer-local-completion-map
   ("M-/" . completion-at-point)))

(use-package company-posframe
  :defer 1
  :config
  (company-posframe-mode))

(use-package prescient
  :defer 1
  :commands
  prescient-persist-mode
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :defer 1
  :after (prescient ivy)
  :commands
  ivy-prescient-mode
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :defer 1
  :after (prescient company)
  :commands
  company-prescient-mode
  :config
  (company-prescient-mode))

(use-package dumb-jump
  :defer 5
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'rg)
  :hook
  (prog-mode-hook . dumb-jump-mode)
  ;; dumb-jump shadows some Eshell key bindings, and is not useful there anyway
  (eshell-mode . (lambda () (dumb-jump-mode -1)))
  :bind
  (:map dumb-jump-mode-map
        ("s-j" . dumb-jump-go-prompt)
        ("s-." . dumb-jump-go)
        ("s-J" . dumb-jump-quick-look)))

(use-package spotlight.el
  :git "https://github.com/cjp/spotlight.el"
  :commands
  (spotlight spotlight-fast)
  :bind
  ("C-c M-s" . spotlight)
  ("C-c M-S" . spotlight-fast))

(bind-key "s-5" #'replace-regexp-entire-buffer-immediately)


;;;; File Management

;; Dired customizations and related packages.

;; Sometimes (depending on how it's compiled and/or where the binary is?)
;; `auto-compression-mode' doesn't load quite right, and then `find-library' and
;; friends can't locate elisp source. Setting up the mode explicitly seems to
;; fix it.

(use-package files
  :ensure nil
  :custom
  (remote-file-name-inhibit-cache nil))

(use-package jka-cmpr-hook
  :defer 1
  :ensure nil
  :commands
  auto-compression-mode
  :config
  (auto-compression-mode))

(use-package epg
  :custom
  (epg-pinentry-mode 'loopback))

(defun upsearch (filename &optional dir)
  "Recursively search up a directory tree for FILENAME.

Start search in DIR or `default-directory'."
  (let ((dir (or dir default-directory)))
    (while (not (or (string= "/" dir)
                    (member filename (directory-files dir))))
      (setq dir (file-name-directory (directory-file-name dir))))
    (unless (string= "/" dir) dir)))

;;;; psync

(defvar-local psync-directory nil
  "Cached directory for `psync'.

It is always buffer local.")

(defun psync-maybe ()
  "If we find a `psync_config' file then run `psync'.

See: https://github.com/mnewt/psync"
  (interactive)
  (when-let
      ((default-directory
         (or psync-directory
             (and default-directory
                  (not (file-remote-p default-directory))
                  (let ((d (shell-command-to-string "git rev-parse --show-toplevel")))
                    (when (and (string-prefix-p "fatal: not a git repository" d)
                               (file-exists-p (expand-file-name "psync_config" d)))
                      d))))))
    (setq psync-directory default-directory)
    (if (= 0 (call-process-shell-command "psync"))
        (message "psync in directory %s finished." default-directory)
      (error "Synchronization with psync failed in directory: %s" default-directory))))

(add-hook 'after-save-hook #'psync-maybe)

(defun psync-clone (source destination)
  "Clone a new repository for use with `psync' from SOURCE to DESTINATION."
  (interactive (list (read-directory-name "Source directory: ")
                     (read-directory-name "Destination directory: ")))
  (async-shell-command (format "psync -v clone '%s' '%s'" source destination)))

;;;; File utils

(defun dos2unix ()
  "Convert DOS line endings to Unix ones."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t)))
  (set-buffer-file-coding-system 'unix 't))

(defun unix2dos ()
  "Convert Unix encoded buffer to DOS encoding.
https://edivad.wordpress.com/2007/04/03/emacs-convert-dos-to-unix-and-vice-versa/"
  (interactive)
  (set-buffer-file-coding-system 'dos))

(defvar touch-history nil
  "History for `touch' command.")

(defun touch (cmd)
  "Run `touch CMD' in `default-directory'."
  (interactive (list (read-shell-command "Run touch (like this): "
                                         "touch "
                                         'touch-history
                                         "touch ")))
  (shell-command cmd))

(defun tail (file)
  "Run `tail -f' on FILE.
Tries to find a file at point."
  (interactive (list (completing-read "Tail file: "
                                      'read-file-name-internal
                                      'file-exists-p t nil 'file-name-history
                                      (thing-at-point 'filename))))
  (let ((cmd (if prefix-arg
                 (read-shell-command "Run tail (like this): "
                                     "tail -f"
                                     'tail-history)
               "tail -f")))
    (async-shell-command (format "%s %s" cmd file))))

(defun df ()
  "Display the local host's disk usage in human readable form."
  (interactive)
  (print (shell-command-to-string "df -h")))

;;;; OS program interaction

(defun reveal-in-windows-explorer (file)
  "Reveal FILE in Windows Explorer."
  (call-process "explorer" nil 0 nil
                (concat "/select," (dired-replace-in-string "/" "\\" file))))

(defun os-reveal-file (&optional file)
  "Reveal FILE using the operating system's GUI file browser."
  (interactive)
  (let ((file (or file buffer-file-name)))
    (message "Revealing %s..." file)
    (pcase system-type
      ('darwin (progn
                 (use-package reveal-in-osx-finder
                   :if (eq system-type 'darwin)
                   :commands
                   (reveal-in-osx-finder))
                 (reveal-in-osx-finder file)))
      ('windows-nt (reveal-in-windows-explorer file))
      ('cygwin (reveal-in-windows-explorer file)))))

(defun os-open-file (&optional file)
  "Open visited FILE in default external program.
When in dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
  (interactive)
  (let* ((file (if (eq major-mode 'dired-mode)
                   (dired-get-file-for-visit)
                 (or file buffer-file-name)))
         (open (pcase system-type
                 ('darwin "open")
                 ((or 'gnu 'gnu/linux 'gnu/kfreebsd) "xdg-open")
                 ((or 'windows-nt 'cygwin) "command")))
         (program (if (or current-prefix-arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (message "Opening %s in the OS registered external program..." file)
    (call-process program nil 0 nil file)))

;;;; Dired

(defun dired-open-file ()
  "Open file at point in OS default program."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (os-open-file file)))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-aFhl")
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-dwim-target t)
  ;; `dired-omit-mode' is managed by `dired-filter'.
  ;; (dired-omit-mode t)
  (dired-omit-files "\\`\\(?:[#.]\\|flycheck_\\).*")
  ;; Try to use GNU ls on macOS since BSD ls doesn't explicitly support
  ;; Emacs and can run into issues with certain characters in the file name.
  (insert-directory-program (or (executable-find "gls")
                                (executable-find "ls")))
  ;; Don't prompt to kill buffers of deleted directories.
  (find-ls-option '("-print0 | xargs -0 ls -alhd" . ""))
  :commands
  dired-summary
  dired-do-delete
  dired-mark
  dired-display-file
  dired-find-file-other-window
  dired-sort-toggle-or-edit
  dired-toggle-marks
  dired-unmark-all-marks
  dired-unmark
  dired-view-file
  dired-ediff-files
  :config
  (defhydra hydra-dired (:hint nil :color pink)
    "
_+_ mkdir          _v_ view         _m_ mark             _(_ details        _i_ insert-subdir
_C_ copy           _O_ view other   _U_ unmark all       _)_ omit-mode      _W_  wdired
_D_ delete         _o_ open other   _u_ unmark           _l_ redisplay      _w_ kill-subdir
_R_ rename         _M_ chmod        _t_ toggle           _g_ revert buf     _e_ ediff
_Y_ rel symlink    _G_ chgrp        _E_ extension mark   _s_ sort           _r_ rsync
_S_ symlink        _z_ compress     _F_ find marked                       _?_ summary
_A_ find regexp    _Q_ repl regexp                                      _q_ quit

C-x C-q : edit     C-c C-c : commit C-c ESC : abort                 _._ toggle hydra
"
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("?" dired-summary)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy) ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer) ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay) ;; relist the marked or single directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-rsync)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file) ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("W" wdired-change-to-wdired-mode)
    ("Y" dired-do-relsymlink)
    ("z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  (dired-mode-hook . (lambda ()
                       (unless (file-remote-p default-directory)
                         (auto-revert-mode))))
  :bind
  (:map dired-mode-map
        ("." . hydra-dired/body)
        ("C-c C-o" . dired-open-file)
        ("T" . touch)
        ("F" . tail-file)
        (";" . dired-git-add)))

(use-package dired-x
  :ensure nil
  :after dired
  :defer 6
  :config
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  :bind
  (:map dired-mode-map
        ("C-." . dired-omit-mode)))

(use-package dired-rsync
  :bind
  (:map dired-mode-map
        ("C-c C-r" . dired-rsync)))

(use-package disk-usage
  :bind
  (:map dired-mode-map
        (")" . disk-usage-here)
        ("C-)" . disk-usage)))

(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t)
  :commands
  (wdired-change-to-wdired-mode)
  :bind
  (:map dired-mode-map
        ("C-c C-p" . wdired-change-to-wdired-mode)))

(use-package dired-hacks-utils
  :defer 5
  :commands
  dired-utils-format-information-line-mode
  :config
  (dired-utils-format-information-line-mode))

(use-package dired-rainbow
  :defer 5
  :after dired-hacks-utils
  :config
  (dired-rainbow-define-chmod directory "#0074d9" "d.*" 'end)
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml") 'end)
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata") 'end)
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx" "xls" "xlsx" "vsd" "vsdx") 'end)
  (dired-rainbow-define markdown "#4dc0b5" ("org" "org_archive" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt") 'end)
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc") 'end)
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac") 'end)
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg") 'end)
  (dired-rainbow-define log "#c17d11" ("log" "log.1" "log.2" "log.3" "log.4" "log.5" "log.6" "log.7" "log.8" "log.9") 'end)
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "fish" "sed" "sh" "zsh" "vim") 'end)
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "hy" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "cljc" "cljx" "edn" "scala" "js" "jsx") 'end)
  (dired-rainbow-define compiled "#6cb2eb" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" "java") 'end)
  (dired-rainbow-define executable "#8cc4ff" ("com" "exe" "msi") 'end)
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar") 'end)
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp") 'end)
  (dired-rainbow-define encrypted "#f2d024" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem") 'end)
  (dired-rainbow-define fonts "#f6993f" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf") 'end)
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak") 'end)
  (dired-rainbow-define vc "#6cb2eb" ("git" "gitignore" "gitattributes" "gitmodules") 'end)
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*" 'end)
  (dired-rainbow-define junk "#7F7D7D" ("DS_Store" "projectile") 'end))

(use-package dired-rainbow-x
  :defer 5
  :after dired-rainbow
  :ensure nil
  :commands
  dired-rainbow-listing-mode
  :config
  (dired-rainbow-listing-mode))

(use-package dired-filter
  :disabled t
  :after dired-hacks-utils
  :custom
  (dired-filter-verbose nil)
  :hook
  (dired-mode-hook . dired-filter-mode))

(use-package dired-list
  :defer 5
  :after dired-hacks-utils
  :git (:uri "https://github.com/Fuco1/dired-hacks"
             :files "dired-list.el")
  :commands
  (dired-list
   dired-list-git-ls-files
   dired-list-locate
   dired-list-find-file
   dired-list-find-name
   dired-list-grep))

(use-package dired-subtree
  :after dired-hacks-utils
  :bind
  (:map dired-mode-map
        ("I" . dired-subtree-cycle)
        ("TAB" . dired-subtree-cycle)
        ("C-, i" . dired-subtree-insert)
        ("C-, r" . dired-subtree-remove)
        ("C-, R" . dired-subtree-revert)
        ("C-, n" . dired-subtree-narrow)
        ("C-, ^" . dired-subtree-up)
        ("C-, v" . dired-subtree-down)))

(use-package dired-collapse
  :after dired-hacks-utils
  :hook
  (dired-mode-hook . dired-collapse-mode))

(use-package dired-sidebar
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  :hook
  (dired-sidebar-mode-hook . (lambda ()
                               (unless (file-remote-p default-directory)
                                 (auto-revert-mode))))
  :bind
  ("C-x C-d" . dired-sidebar-toggle-sidebar))

(defun dired-list-init-files ()
  "List Emacs init files."
  (interactive)
  (git-home-link "dotemacs")
  (dired-list-git-ls-files user-emacs-directory)
  (when (bound-and-true-p dired-omit-mode) (dired-omit-mode -1)))

(defun dired-list-dotfiles ()
  "List Emacs init files."
  (interactive)
  (git-home-link "dotfiles")
  (dired-list-git-ls-files "~"))

(use-package counsel-tramp
  :hook
  (counsel-tramp-pre-command-hook . (lambda () (projectile-mode 0)))
  (counsel-tramp-quit-hook . projectile-mode)
  :commands
  counsel-tramp)

(bind-keys
 ("C-x M-s" . psync-maybe)
 ("C-c o" . os-open-file)
 ("C-c O" . os-reveal-file)
 :map m-toggle-map
 ("r" . auto-revert-mode))


;;;; Network

;; Network utilities.

;; Automate communication with services, such as nicserv.
(use-package erc
  :hook
  (erc-connect-pre-hook . erc-services-mode))

(use-package url
  :config
  (defun public-ip ()
    "Display the local host's apparent public IP address."
    (interactive)
    (url-retrieve "https://diagnostic.opendns.com/myip"
                  (lambda (_)
                    (goto-char (point-min))
                    (re-search-forward "^$")
                    (delete-char 1)
                    (delete-region (point) (point-min))
                    (let ((ip (buffer-string)))
                      (kill-new ip)
                      (message ip)))))
  :commands
  (public-ip url-retrieve))

(use-package request
  :commands
  request)

(defun dis (hostname)
  "Resolve a HOSTNAME to its IP address."
  (interactive "MHostname: ")
  (message (shell-command-to-string
            (concat "drill "
                    hostname
                    " | awk '/;; ANSWER SECTION:/{flag=1;next}/;;/{flag=0}flag'"))))

(defun ips ()
  "Show the machine's IP addresses."
  (interactive)
  (shell-command
   (pcase system-type
     ('gnu/linux
      "ip address show | awk '/inet /{if ($5 != \"lo\") { print $7 \": \" $2 }}'")
     ('darwin
      "/sbin/ifconfig | awk '/^[a-z0-9]+:/{ i=$1 } /inet / { if (i != \"lo0:\") { print i \" \" $2 }}'")
     ('cygwin
      "ipconfig | awk -F' .' '/Address/ {print $NF}'"))))

(use-package wttrin
  :custom
  (wttrin-default-cities '("Albany CA"
                           "San Francisco CA"
                           "Austin TX"
                           "Eugene OR"
                           "Truckee CA"
                           "Moon"))
  (wttrin-default-accept-language '("Accept-Language" . "en-US"))
  :config
  (defun advice-delete-other-windows (&rest _)
    "Advice that will delete other windows."
    (delete-other-windows))

  (advice-add 'wttrin :before #'advice-delete-other-windows)
  :bind
  ("C-c M-w" . wttrin))

;;;; mnt

;; These functions execute the `mnt' utility, which uses config
;; profiles to mount smb shares (even through ssh tunnels).

(defun mnt-cmd (cmd)
  "Interactively Run a `mnt/umnt' utility (CMD).
The config is specified in the config file in `~/.mnt/'."
  (let ((config (completing-read (format "Run %s using config: " cmd)
                                 (directory-files "~/.mnt" nil "^[^.]")
                                 nil t)))
    (setq config (expand-file-name config "~/.mnt"))
    (if (async-shell-command (concat cmd " " config) "*mnt*")
        (message (format "%s succeeded with config file: %s" cmd config))
      (message (format "%s FAILED with config file: %s" cmd config)))))

(defun mnt ()
  "Mount a share using the `mnt' utility."
  (interactive)
  (mnt-cmd "sudo_mnt"))

(defun umnt ()
  "Unmount a share using the `umnt' utility."
  (interactive)
  (mnt-cmd "umnt"))


;;;; Version Control

;; make sure vc stuff is not making tramp slower
(use-package vc
  :custom
  (vc-ignore-dir-regexp (format "%s\\|%s"
                                vc-ignore-dir-regexp
                                tramp-file-name-regexp)))

(defun git-ls-files (&optional directory)
  "Return a list of the files from `git ls-files DIRECTORY'."
  (split-string (shell-command-to-string
                 (concat "git ls-files " (or directory default-directory)))))

(defun git-add-current-file (file)
  "Run `git add' on the FILE visited in the current buffer."
  (interactive (list (buffer-file-name)))
  (let ((dir (s-trim (shell-command-to-string "git rev-parse --show-toplevel"))))
    (if (= 0 (call-process-shell-command (concat "git add " file)))
        (message "File %s was added to the git repo at %s." (buffer-file-name) dir)
      (error "Failed to add file %s to the git repo at %s" (buffer-file-name) dir))))

;; TODO: Figure out why byte compiler thinks this is getting defined twice.
;; (defun dired-git-add ()
;;   "Run `git add' on the selected files in a dired buffer."
;;   (interactive)
;;   (let ((files (dired-get-marked-files)))
;;     (message "> git add %s" files)
;;     (dired-do-shell-command "git add" nil files)
;;     (dired-revert)))

(defvar git-home-repo-dir
  (expand-file-name "repos" (or (getenv "XDG_CONFIG_HOME") "~/.config")))

(defun git-worktree-link (gitdir worktree)
  "Link git WORKTREE at GITDIR.
https://github.com/magit/magit/issues/460#issuecomment-36139308"
  (interactive (list (read-directory-name "Gitdir: ")
                     (read-directory-name "Worktree: ")))
  (with-temp-file (expand-file-name ".git" worktree)
    (insert "gitdir: " (file-relative-name gitdir worktree) "\n"))
  (magit-call-git "config" "-f" (expand-file-name "config" gitdir)
                  "core.worktree" (file-relative-name worktree gitdir))
  ;; Configure projectile to only look at tracked files
  (if (boundp 'projectile-git-command)
      (setq projectile-git-command "git ls-files -zc --exclude-standard")))

(defun git-worktree-unlink (worktree)
  "Unlink git WORKTREE at GITDIR."
  (interactive (list (read-directory-name "Worktree: ")))
  ;; Configure projectile back to default, which looks for all non-ignored files
  (if (boundp 'projectile-git-command)
      (setq projectile-git-command "git ls-files -zco --exclude-standard"))
  ;; This does `git config --unset core.worktree'.  We don't actually
  ;; have to do this and not doing it would have some advantages, but
  ;; might be confusing.
  ;; (magit-set nil "core.worktree")
  ;; This causes an error if this actually is a directory, which is
  ;; a good thing, it saves us from having to do this explicitly :-)
  (delete-file (expand-file-name ".git" worktree)))

(defun git-home-link (repo)
  "Interactively link a git REPO's worktree to $HOME."
  (interactive (list (completing-read "Link git home repository: "
                                      (directory-files git-home-repo-dir nil "^[^.]")
                                      nil t)))
  (setq repo (expand-file-name repo git-home-repo-dir))
  (git-worktree-link repo (getenv "HOME"))
  (message "Linked repo at %s" repo))

(defun git-home-unlink ()
  "Unlink the current git repo's worktree from $HOME."
  (interactive)
  (let ((f (expand-file-name ".git" (getenv "HOME"))))
    (git-worktree-unlink (getenv "HOME"))
    (message "Unlinked repo at %s" f)))

(with-eval-after-load 'vc
  ;; VC follows the link and visits the real file, telling you about it in the
  ;; echo area.
  (setq vc-follow-symlinks t
        ;; Backup even if file is in vc.
        vc-make-backup-files t))

(use-package magit
  :custom
  (magit-repository-directories `((,code-directory . 1)))
  (magit-completing-read-function 'ivy-completing-read)
  :config
  (use-package forge :demand t)

  :commands
  magit-call-git
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch))

(use-package magit-todos
  :commands
  magit-todos--scan-with-git-grep
  :custom
  (magit-todos-scanner #'magit-todos--scan-with-git-grep)
  :hook
  (magit-mode-hook . magit-todos-mode))

(use-package git-timemachine
  :bind
  ("C-x t" . git-timemachine))

;; (use-package gist
;;   :commands
;;   gist-list)

(use-package diff-hl
  :commands
  (diff-hl-magit-post-refresh diff-hl-mode diff-hl-dired-mode)
  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  ((prog-mode-hook markdown-mode-hook) . diff-hl-mode)
  (dired-mode-hook . diff-hl-dired-mode))

(use-package smerge-mode
  :config
  (defhydra hydra-smerge (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))

  :bind
  (:map smerge-mode-map
        ("C-s-s" . hydra-smerge/body)))

(bind-keys
 ("M-m l" . git-home-link)
 ("M-m u" . git-home-unlink)
 ("C-x G" . projectile-git-ls-files-dired)
 :map m-file-map
 (";" . git-add-current-file))


;;;; Editing

;; General editing related configuration and functionality

;; Use the system clipboard
(setq select-enable-clipboard t
      ;; Save existing system clipboard text into kill ring before replacing it,
      ;; ensuring it doesn't get irrevocably destroyed.
      save-interprogram-paste-before-kill t
      ;; use mouse to kill/yank
      mouse-yank-at-point t
      mouse-drag-and-drop-region t
      mouse-drag-and-drop-region-cut-when-buffers-differ t)

;; Wrap text.
(setq-default fill-column 80)

;; Newline at end of file.
(setq require-final-newline t
      ;; Sentences end with one space.
      sentence-end-double-space nil)

;; Tabs
(setq-default indent-tabs-mode nil
              tab-width 2
              tab-stop-list (number-sequence tab-width 120 tab-width))

;; Delete selection on insert or yank
(use-package delsel
  :defer 1
  :commands
  delete-selection-mode delete-active-region
  :config
  (delete-selection-mode))

;; Automatically indent after RET
(use-package electric
  :defer 1
  :commands
  electric-indent-mode
  :config
  (electric-indent-mode))

(defun auto-fill-mode-init ()
  "Automatically fill comments. Wraps on `fill-column' columns."
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'prog-mode-hook #'auto-fill-mode-init)

(use-package so-long
  :defer 1
  :if (>= emacs-major-version 27)
  :ensure nil
  :config
  (global-so-long-mode))

(use-package unfill
  :commands
  (unfill-region unfill-paragraph)
  :bind
  (:map prog-mode-map
        ("M-q" . unfill-toggle)))

;; http://whattheemacsd.com/key-bindings.el-03.html
(defun delete-indentation-forward ()
  "Like `delete-indentation', but in the opposite direction.
Bring the line below point up to the current line."
  (interactive)
  (join-line -1))

(use-package undo-redo
  :git "https://github.com/clemera-dev/undo-redo"
  :bind
  ("s-z" . undo-modern)
  ("s-Z" . redo))

(use-package undohist
  :git "https://github.com/clemera-dev/undohist"
  :defer 1
  :config
  (undohist-initialize))

(use-package undo-propose
  :bind
  ("M-s-z" . undo-propose))

(use-package volatile-highlights
  :defer 2
  :config
  (vhl/define-extension 'undo-redo 'undo-modern 'undo)
  (vhl/install-extension 'undo-redo)
  (volatile-highlights-mode t))

(use-package goto-chg
  :defer 1
  :bind
  ("C-." . goto-last-change)
  ("C-;" . goto-last-change-reverse))

(use-package easy-kill
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark)))

(use-package mwim
  :bind
  ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
  ([remap move-end-of-line] . mwim-end-of-code-or-line))

(use-package expand-region
  :after org
  :custom
  (expand-region-fast-keys-enabled nil)
  :bind
  ("s-d" . er/expand-region)
  ("C-=" . er/expand-region)
  ("s-D" . er/contract-region)
  ("C-+" . er/contract-region))

(use-package multiple-cursors
  :commands
  mc/add-cursor-on-click
  :config
  (defhydra hydra-multiple-cursors (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
_p_   Next    _n_   Next    _l_ Edit lines
_P_   Skip    _N_   Skip    _a_ Mark all
_M-p_ Unmark  _M-n_ Unmark  _r_ Mark by regexp
^ ^             ^ ^             _q_ Quit
"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil)
    ("<mouse-1>" mc/add-cursor-on-click)
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore))

  :bind
  ("M-<down-mouse-1>" . mc/add-cursor-on-click)
  ("C-S-c C-S-c" . mc/edit-lines)
  ("M-s-m" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/unmark-next-like-this)
  ("C-M-<" . mc/mark-previous-like-this)
  ("C-M->" . mc/unmark-previous-like-this)
  ("C-c >" . mc/mark-all-dwim)
  ("C-c C-a"  . mc/mark-all-dwim)
  ("C-'" . mc-hide-unmatched-lines-mode))

(use-package replace
  :ensure nil
  :commands
  occur-next
  occur-prev
  :config
  (defun occur-dwim ()
    "Call `occur' with a sane default, chosen as the thing under point or selected region."
    (interactive)
    (push (if (region-active-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))
            (let ((sym (thing-at-point 'symbol)))
              (when (stringp sym)
                (regexp-quote sym))))
          regexp-history)
    (call-interactively 'occur))

  (declare-function other-window-hydra-occur 'hydra)

  (advice-add 'occur-mode-goto-occurrence :after #'other-window-hydra-occur)

  ;; Focus on *Occur* window right away.
  (add-hook 'occur-hook (lambda () (other-window 1)))

  (defun reattach-occur ()
    "Switch to Occur buffer and launch the hydra."
    (if (get-buffer "*Occur*")
        (switch-to-buffer-other-window "*Occur*")
      (hydra-occur-dwim/body)))

  ;; Used in conjunction with occur-mode-goto-occurrence-advice this helps keep
  ;; focus on the *Occur* window and hides upon request in case needed later.
  (defhydra hydra-occur-dwim ()
    "Occur mode"
    ("o" occur-dwim "Start occur-dwim" :color red)
    ("j" occur-next "Next" :color red)
    ("k" occur-prev "Prev":color red)
    ("h" delete-window "Hide" :color blue)
    ("r" (reattach-occur) "Re-attach" :color red)
    ("q" nil))

  :bind
  (:map occur-mode-map
        ("C-o" . hydra-occur-dwim/body)))

(use-package move-text
  :bind
  (:map prog-mode-map
        ("M-S-<up>" . move-text-up)
        ("M-S-<down>" . move-text-down)))

(use-package string-inflection
  :bind
  ("C-c C-u" . string-inflection-all-cycle))

(use-package yasnippet
  :defer 5
  :custom
  ;; Don't write messages at startup.
  (yas-verbosity 1)
  :commands
  yas-global-mode
  :config
  (yas-global-mode)
  :bind
  (:map yas-minor-mode-map
        ("s-'" . yas-expand)
        ("C-c C-s" . yas-insert-snippet)))

(use-package yasnippet-snippets
  :defer 5)

(use-package parinfer
  :custom
  (parinfer-extensions
   '(defaults       ; should be included.
      pretty-parens ; different paren styles for different modes.
      smart-tab     ; C-b & C-f jump positions and smart shift with tab & S-tab.
      smart-yank))  ; Yank behavior depends on mode.
  :config
  (parinfer-strategy-add 'default 'newline-and-indent)
  :hook
  ((clojure-mode-hook
    emacs-lisp-mode-hook
    hy-mode-hook
    lisp-interaction-mode-hook
    lisp-mode-hook
    scheme-mode-hook) . parinfer-mode)
  :commands
  (parinfer-strategy-add parinfer--invoke-parinfer)
  :bind
  (:map parinfer-mode-map
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)
        ("C-," . parinfer-toggle-mode)
        ;; Don't interfere with smartparens quote handling
        ("\"" . nil)
        ;; sp-newline seems to offer a better experience for lisps
        ("RET" . nil)
        ("<return>" . nil)
        :map parinfer-region-mode-map
        ("C-i" . indent-for-tab-command)
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)))

(use-package smartparens
  :defer 1
  :custom
  ;; Don't kill the entire symbol with `sp-kill-hybrid-sexp'. If we want to kill
  ;; the entire symbol, use `sp-kill-symbol'.
  (sp-hybrid-kill-entire-symbol nil)
  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (sp-cancel-autoskip-on-backward-movement nil)
  :commands
  sp-get-pair
  sp--get-opening-regexp
  sp--get-closing-regexp
  :config
  (eval-when-compile (require 'sh-script))

  (defun sp-add-space-after-sexp-insertion (id action _context)
    "Add space after sexp insertion.
ID, ACTION, CONTEXT."
    (when (eq action 'insert)
      (save-excursion
        (forward-char (sp-get-pair id :cl-l))
        (when (or (eq (char-syntax (following-char)) ?w)
                  (looking-at (sp--get-opening-regexp)))
          (insert " ")))))

  (defun sp-add-space-before-sexp-insertion (id action _context)
    "Add space before sexp insertion.
ID, ACTION, CONTEXT."
    (when (eq action 'insert)
      (save-excursion
        (backward-char (length id))
        (when (or (eq (char-syntax (preceding-char)) ?w)
                  (and (looking-back (sp--get-closing-regexp) nil)
                       (not (eq (char-syntax (preceding-char)) ?'))))
          (insert " ")))))

  (defun sp-sh-post-handler (_id action _context)
    "Bash post handler.
ID, ACTION, CONTEXT."
    (-let (((&plist :arg arg :enc enc) sp-handler-context))
      (when (equal action 'barf-backward)
        (delete-indentation 1)
        (indent-according-to-mode)
        (save-excursion
          (sp-backward-sexp)
          (sp-backward-sexp arg)
          (just-one-space)))
      (when (equal action 'barf-forward)
        (sp-get enc
          (let ((beg-line (line-number-at-pos :beg-in)))
            ;; (end-line (line-number-at-pos :end-in)))
            (sp-forward-sexp arg)
            (just-one-space)
            (when (not (= (line-number-at-pos) beg-line))
              (delete-indentation -1))
            (indent-according-to-mode))))
      (when (eq action 'insert)
        (save-excursion
          (indent-according-to-mode)))))

  (defun sp-sh-for-post-handler (id action context)
    "Handler for bash for block insertions.
ID, ACTION, CONTEXT."
    (when (equal action 'insert)
      (save-excursion
        (insert (format " in list; do\n%s\n" (make-string sh-basic-offset ?\s)))))
    (sp-sh-post-handler id action context))

  (defun sp-sh-if-post-handler (_id action _context)
    "Handler for bash if block insertions.
ID, ACTION, CONTEXT."
    (when (equal action 'insert)
      (save-excursion
        (insert (format " ; then\n%s\n" (make-string sh-basic-offset ?\s))))))

  (defun sp-sh-case-post-handler (id action context)
    "Handler for bash case block insertions.
ID, ACTION, CONTEXT."
    (when (equal action 'insert)
      (save-excursion
        (insert (format " in\n%s\*)\n%s\n%s;;\n"
                        (make-string sh-basic-offset ?\s)
                        (make-string (* 2 sh-basic-offset) ?\s)
                        (make-string (* 2 sh-basic-offset) ?\s)))))
    (sp-sh-post-handler id action context))

  (declare-function thing-at-point-looking-at 'thingatpt)

  (defun sp-sh-pre-handler (_id action _context)
    "Handler for sh slurp and barf.
ID, ACTION, CONTEXT."
    (let ((enc (plist-get sp-handler-context :enc)))
      (sp-get enc
        (let ((beg-line (line-number-at-pos :beg-in))
              (end-line (line-number-at-pos :end-in)))
          (when (equal action 'slurp-backward)
            (save-excursion
              (sp-forward-sexp)
              (when (looking-at-p ";") (forward-char))
              (just-one-space)
              (when (not (= (line-number-at-pos) end-line))
                (delete-indentation -1))))
          (when (equal action 'barf-backward)
            ;; Barf whole method chains
            (while (thing-at-point-looking-at "[(.:[][\n[:blank:]]*")
              (sp-forward-sexp))
            (if (looking-at-p " *$")
                (newline)
              (save-excursion (newline))))
          (when (equal action 'slurp-forward)
            (save-excursion
              (sp-backward-sexp)
              (just-one-space)
              (when (not (= (line-number-at-pos) beg-line))
                (delete-indentation)))
            (when (looking-at-p ";") (forward-char))
            (if (= (line-number-at-pos) beg-line)
                (insert " ")
              (newline)))
          (when (equal action 'barf-forward)
            (if (= (line-number-at-pos) end-line)
                (insert " ")
              (if (looking-back "^[[:blank:]]*" nil)
                  (save-excursion (newline))
                (newline))))))))

  (defun sp-sh-insert-spaces (_id action _context)
    "Handler for sh wrap.
ID, ACTION, CONTEXT."
    (when (eq action 'wrap)
      (save-excursion
        (goto-char (sp-get sp-last-wrapped-region :end-in))
        (unless (looking-at "]]")
          (insert " "))
        (goto-char (sp-get sp-last-wrapped-region :beg-in))
        (unless (looking-back "\\[\\[" nil)
          (insert " ")))))

  (defun sp-backward-slurp-into-previous-sexp ()
    "Add the sexp at point into the preceeding list."
    (interactive)
    (save-excursion
      (sp-down-sexp)
      (sp-backward-symbol)
      (sp-forward-slurp-sexp)))

  (defun sp-create-newline-and-enter-sexp (&rest _)
    "Open a new brace or bracket expression, with relevant newlines and indent.

See https://github.com/Fuco1/smartparens/issues/80."
    (message "sp-newline-etc")
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (require 'smartparens-config)

  ;; Smartparens is broken in `cc-mode' as of Emacs 27. See
  ;; https://github.com/Fuco1/smartparens/issues/963
  (when (version<= "27" emacs-version)
    (dolist (fun '(c-electric-paren c-electric-brace))
      (add-to-list 'sp--special-self-insert-commands fun)))

  (sp-with-modes '(c-mode c++-mode csharp-mode css-mode graphql-mode
                          javascript-mode js-mode js2-mode json-mode
                          objc-mode java-mode web-mode)
    (sp-local-pair "{" nil
                   :post-handlers
                   '((sp-create-newline-and-enter-sexp "RET" newline-and-indent)))
    (sp-local-pair "[" nil
                   :post-handlers
                   '((sp-create-newline-and-enter-sexp "RET" newline-and-indent)))
    (sp-local-pair "(" nil
                   :post-handlers
                   '((sp-create-newline-and-enter-sexp "RET" newline-and-indent))))
  (sp-with-modes 'python-mode
    (sp-local-pair "\"\"\"" "\"\"\""
                   :post-handlers
                   '((sp-create-newline-and-enter-sexp "RET" newline-and-indent))))
  (sp-with-modes 'sh-mode
    (sp-local-pair "{" nil
                   :post-handlers
                   '((sp-create-newline-and-enter-sexp "RET" newline-and-indent)
                     sp-sh-post-handler))
    (sp-local-pair "(" nil
                   :post-handlers
                   '((sp-create-newline-and-enter-sexp "RET" newline-and-indent)
                     sp-sh-post-handler))
    (sp-local-pair "[" "]"
                   :actions '(wrap insert navigate)
                   :post-handlers '(sp-sh-insert-spaces sp-sh-post-handler))
    (sp-local-pair "[ " " ]"
                   :actions '(wrap insert navigate)
                   :post-handlers '(sp-sh-insert-spaces sp-sh-post-handler))
    (sp-local-pair "[[" "]]"
                   :actions '(wrap insert navigate)
                   :post-handlers '(sp-sh-insert-spaces sp-sh-post-handler))
    (sp-local-pair "[[ " " ]]" :actions '(wrap insert navigate)
                   :post-handlers '(sp-sh-insert-spaces sp-sh-post-handler))
    (sp-local-pair "for" "done"
                   :when '(("SPC" "RET" "TAB" sp-newline))
                   :unless '(sp-in-string-p sp-in-comment-p)
                   :actions '(insert navigate)
                   :pre-handlers '(sp-sh-pre-handler)
                   :post-handlers '(sp-sh-for-post-handler))
    (sp-local-pair "if" "fi"
                   :when '(("SPC" "RET" "TAB" sp-newline))
                   :unless '(sp-in-string-p sp-in-comment-p)
                   :actions '(insert navigate)
                   :pre-handlers '(sp-sh-pre-handler)
                   :post-handlers '(sp-sh-if-post-handler))
    (sp-local-pair "case" "esac"
                   :when '(("SPC" "RET" "TAB" sp-newline))
                   :unless '(sp-in-string-p sp-in-comment-p)
                   :actions '(insert navigate)
                   :pre-handlers '(sp-sh-pre-handler)
                   :post-handlers '(sp-sh-case-post-handler)))
  (sp-with-modes 'org-mode
    (sp-local-pair "=" "=" :wrap "C-M-=")
    (sp-local-pair "~" "~" :wrap "C-~"))
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-with-modes sp-lisp-modes
    (sp-local-pair "(" nil
                   :wrap "C-M-("
                   :pre-handlers '(sp-add-space-before-sexp-insertion)
                   :post-handlers '(sp-add-space-after-sexp-insertion)))
  (setq sp-ignore-modes-list
        (delete 'minibuffer-inactive-mode sp-ignore-modes-list))
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  :commands
  sp-local-pair
  sp-with-modes
  smartparens-global-mode
  show-smartparens-global-mode
  sp-point-in-string-or-comment sp-forward-slurp-sexp
  sp-backward-symbol sp-backward-symbol sp-down-sexp
  sp-forward-sexp sp-backward-sexp
  :bind
  (:map lisp-mode-shared-map
        ("RET" . sp-newline)
        ("<return>" . sp-newline)
        ("C-k" . sp-kill-hybrid-sexp)
        (";" . sp-comment))
  (:map smartparens-mode-map
        ("M-s" . nil)
        ("M-r" . nil)
        ("M-<up>" . nil)
        ("M-<down>" . nil)
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-n" . sp-next-sexp)
        ("C-M-p" . sp-previous-sexp)
        ("M-a" . sp-beginning-of-sexp)
        ("M-e" . sp-end-of-sexp)
        ("C-M-d" . sp-down-sexp)
        ("C-M-u" . sp-backward-up-sexp)
        ("M-s-s" . sp-splice-sexp)
        ("C-S-d" . sp-kill-symbol)
        ("C-M-k" . sp-kill-sexp)
        ("C-M-w" . sp-copy-sexp)
        ("C-M-t" . sp-transpose-sexp)
        ("C-M-SPC" . sp-mark-sexp)
        ("M-<backspace>" . sp-backward-kill-word)
        ("C-<backspace>" . sp-backward-kill-symbol)
        ("C-M-<backspace>" . sp-backward-kill-sexp)
        ("C-s-<backspace>" . sp-splice-sexp-killing-backward)
        ("M-(" . sp-wrap-round)
        ("M-[" . sp-wrap-square)
        ("M-{" . sp-wrap-qurly)
        ("M-<delete>" . sp-unwrap-sexp)
        ("M-<backspace>" . sp-backward-delete-word)
        ("C-)" . sp-forward-slurp-sexp)
        ("C-}" . sp-forward-barf-sexp)
        ("C-(" . sp-backward-slurp-sexp)
        ("C-{" . sp-backward-barf-sexp)
        ("M-k" . sp-split-sexp)
        ("M-j" . sp-join-sexp)
        ("C-c s a" . sp-absorb-sexp)
        ("C-c s e" . sp-emit-sexp)
        ("C-c s p" . sp-convolute-sexp)
        ("C-c s t" . sp-transpose-hybrid-sexp)
        ("C-c s (" . sp-rewrap-sexp)
        ("C-c s r" . sp-change-inner)
        ("C-c s s" . sp-change-enclosing)))

(defun clipboard-yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (if (and delete-selection-mode (use-region-p)) (delete-active-region))
  (clipboard-yank)
  (call-interactively 'indent-region))

(defun kill-line-or-region ()
  "Kill the current line or active region.

When `universal-argument' is called first, kill the whole
buffer (respects `narrow-to-region').

Stolen from `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun copy-line-or-region ()
  "Copy the current line or active region.

When called repeatedly, append copy subsequent lines. When
`universal-argument' is called first, copy whole
buffer (respects`narrow-to-region').

Stolen from `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'"
  (interactive)
  (if current-prefix-arg
      (progn
        (kill-ring-save (point-min) (point-max)))
    (if (use-region-p)
        (progn
          (kill-ring-save (region-beginning) (region-end)))
      (if (eq last-command this-command)
          (if (eobp)
              (progn)
            (progn
              (kill-append "\n" nil)
              (kill-append
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))
               nil)
              (progn
                (end-of-line)
                (forward-char))))
        (if (eobp)
            (if (eq (char-before) 10)
                (progn)
              (progn
                (kill-ring-save (line-beginning-position) (line-end-position))
                (end-of-line)))
          (progn
            (kill-ring-save (line-beginning-position) (line-end-position))
            (end-of-line)
            (forward-char)))))))

(defun comment-toggle ()
  "Toggle comments for the region.
If no region is selected, toggles comments for the line."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end))
  (if (bound-and-true-p parinfer-mode) (parinfer--invoke-parinfer)))

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (beginning-of-line)
  (set-mark (line-end-position)))

(defun move-line-or-region-to-other-window ()
  "Kill region or line then yank at point in the other window."
  (interactive)
  (kill-line-or-region)
  (other-window 1)
  (yank)
  (newline)
  (other-window -1))

(defun copy-line-or-region-to-other-window ()
  "Copy region line to point in the other window."
  (interactive)
  (copy-line-or-region)
  (other-window 1)
  (yank)
  (newline)
  (other-window -1))

(bind-keys
 ("C-M-}" . forward-sentence)
 ("C-M-{" . backward-sentence)
 ("C-^" . delete-indentation-forward)
 ("s-C" . copy-line-or-region-to-other-window)
 ("s-X" . move-line-or-region-to-other-window)
 ;; Replace `delete-horizontal-space' with the more useful `cycle-spacing'.
 ("M-\\" . cycle-spacing)
 ;; Continue comment on next line (default binding is "C-M-j")
 ("M-RET" . indent-new-comment-line)
 ("M-z" . zap-to-char)
 ("M-Z" . zap-up-to-char))


;;;; Shell

;; Shell, Term, Tramp, Shell scripting, and related things.

(use-package comint
  :ensure nil
  :custom
  (comint-buffer-maximum-size 20000)
  (comint-prompt-read-only t))

(use-package ssh
  :custom
  (ssh-directory-tracking-mode 'ftp)
  :commands
  ssh)

(defun tramp-cleanup-all ()
  "Clean up all tramp buffers and connections."
  (interactive)
  (cl-letf (((symbol-function #'password-reset)
             (lambda ())))
    (tramp-cleanup-all-connections))
  (setq ivy-history
        (seq-remove (lambda (s) (file-remote-p (substring-no-properties s)))
                    ivy-history)))

(defun tramp-insert-remote-part ()
  "Insert current tramp prefix at point."
  (interactive)
  (if-let* ((remote (file-remote-p default-directory)))
      (insert remote)))

;; Configure TRAMP to respect the PATH variable on the remote machine (for
;; remote eshell sessions)
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(defun expand-environment-variable ()
  "Insert contents of an envionment variable at point."
  (interactive)
  (insert (getenv (read-envvar-name "Insert Environment Variable: "))))

(defun list-hosts-from-known-hosts ()
  "Return a list of hosts from `~/.ssh/known_hosts'."
  (with-temp-buffer
    (insert-file-contents "~/.ssh/known_hosts")
    (cl-remove-if (lambda (host) (string= "" host))
                  (mapcar (lambda (line) (replace-regexp-in-string
                                          "\\]\\|\\[" ""
                                          (car (split-string line "[, :]"))))
                          (split-string (buffer-string) "\n")))))

(defun list-hosts-from-ssh-config ()
  "Return a list of hosts from `~/.ssh/config'."
  (with-temp-buffer
    (insert-file-contents "~/.ssh/config")
    (keep-lines "^Host")
    (cl-remove-if (lambda (host) (or (string=  "" host) (string= "*" host)))
                  (mapcar (lambda (line) (replace-regexp-in-string "Host +" "" line))
                          (split-string (buffer-string) "\n")))))

(defun list-hosts-from-etc-hosts ()
  "Return a list of hosts from `/etc/hosts'."
  (with-temp-buffer
    (insert-file-contents "/etc/hosts")
    (flush-lines "^#")
    (flush-lines "^$")
    (cl-remove-if (lambda (host) (or (string= host "localhost")
                                     (string= host "broadcasthost")
                                     (eq host nil)))
                  (mapcar (lambda (line) (cadr (split-string line "[ \t]+")))
                          (split-string (buffer-string) "\n")))))

(defun list-hosts-from-recentf ()
  "Return a list of hosts from the `recentf-list'."
  (cl-remove-duplicates
   (mapcar (lambda (s)
             (replace-regexp-in-string
              ":.*" ""
              (replace-regexp-in-string "^/sshx\?:" "" s)))
           (cl-remove-if
            (apply-partially #'string-match "^/sshx\?:\\([a-z]+\\):")
            recentf-list))))

(defun ssh-choose-host (&optional prompt)
  "Make a list of recent ssh hosts and interactively choose one with optional PROMPT."
  (completing-read (or prompt "SSH to Host: ")
                   (-distinct
                    (append
                     (list-hosts-from-recentf)
                     (list-hosts-from-known-hosts)
                     (list-hosts-from-ssh-config)
                     (list-hosts-from-etc-hosts)))))

(defun dired-tramp (host)
  "Choose an ssh HOST and then open it with dired."
  (interactive (list (ssh-choose-host "Hostname or tramp string: ")))
  (find-file
   (if (tramp-file-name-p host)
       host
     (find-file (concat "/ssh:" host ":")))))

(use-package shell
  :commands
  comint-delchar-or-maybe-eof
  :config
  ;; http://whattheemacsd.com/setup-shell.el-01.html
  (defun comint-delchar-or-eof-or-kill-buffer (arg)
    "`C-d' on an empty line in the shell terminates the process, accepts ARG."
    (interactive "p")
    (if (null (get-buffer-process (current-buffer)))
        (kill-buffer)
      (comint-delchar-or-maybe-eof arg)))

  (defun shell-rename-buffer (_)
    "Rename buffer to `default-directory'."
    (rename-buffer (format "*Shell: %s*" default-directory) t))
  :commands
  shell
  :hook
  (shell-mode-hook . shell-dirtrack-mode)
  (comint-output-filter-functions . shell-rename-buffer)
  :bind
  (:map shell-mode-map
        ("C-d" . comint-delchar-or-eof-or-kill-buffer)
        ("SPC" . comint-magic-space)
        ("M-r" . counsel-shell-history)))

(use-package bash-completion
  :custom
  ;; So that it doesn't sometimes insert a space ('\ ') after completing the
  ;; file name.
  (bash-completion-nospace t)
  :hook
  (shell-dynamic-complete-functions . bash-completion-dynamic-complete))

(use-package fish-completion
  :ensure-system-package fish
  :custom
  (fish-completion-fallback-on-bash-p t)
  :hook
  ((eshell-mode-hook shell-mode-hook) . fish-completion-mode))

(use-package company-shell
  :config
  (add-to-list
   'company-backends
   `(company-shell company-shell-env
                   ,(when (executable-find "fish") 'company-fish-shell))))
;; dtach (https://github.com/crigler/dtach)
;; https://emacs.stackexchange.com/questions/2283/attach-to-running-remote-shell-with-eshell-tramp-dtach
(defvar explicit-dtach-args
  '("-A" "/tmp/emacs.dtach" "-z" "bash" "--noediting" "--login")
  "Args for dtach.")

(defun ssh-dtach (host)
  "Open SSH connection to HOST and create or attach to dtach session."
  (interactive (list (ssh-choose-host "SSH using dtach to host: ")))
  (let ((explicit-shell-file-name "dtach")
        (default-directory (format  "/ssh:%s:" host)))
    (shell (format "*ssh (dtach) %s*" host))))

(defun sudo-toggle--add-sudo (path)
  "Add sudo to file PATH string."
  (if (file-remote-p path)
      (with-parsed-tramp-file-name (expand-file-name path) nil
        (concat "/" method ":"
                (when user (concat user "@"))
                host "|sudo:root@" host ":" localname))
    (concat "/sudo:root@localhost:" (expand-file-name path))))

(defun sudo-toggle--remove-sudo (path)
  "Remove sudo from file PATH string."
  (cond
   ((string-match-p "/sudo:root@localhost:" path)
    (replace-regexp-in-string (getenv "HOME") "~" (substring path 21)))

   ((string-match-p "|sudo:root@" path)
    (replace-regexp-in-string "|sudo:root@[^:]*" "" path))))

(defun sudo-toggle ()
  "Reopen the current file, directory, or shell as root.

For files and dired buffers, the non-sudo buffer is replaced with
a sudo buffer.

For shells, a sudo shell is opened but the
non-sudo shell is left intact."
  (interactive)
  (let* ((position (point))
         (f (expand-file-name (or buffer-file-name default-directory)))
         (newf (if (string-match-p "sudo:" f)
                   (sudo-toggle--remove-sudo f)
                 (sudo-toggle--add-sudo f)))
         ;; So that you don't get method overrides.
         (tramp-default-proxies-alist nil))
    (cond ((or buffer-file-name (derived-mode-p 'dired-mode))
           (find-file newf)
           (goto-char position))
          ((derived-mode-p 'shell-mode)
           (if (string-match-p "*shell/sudo:root@" (buffer-name))
               (kill-buffer-and-window)
             (with-temp-buffer
               (cd newf)
               (shell (format "*shell/sudo:root@%s*"
                              (with-parsed-tramp-file-name newf nil host))))))
          ((derived-mode-p 'eshell-mode)
           (eshell-return-to-prompt)
           (insert (concat "cd '" newf "'"))
           (eshell-send-input))
          (t (message "Can't sudo this buffer")))))

(defun filter-functions (regexp &optional predicate)
  "Return a list of functions matching REGEXP.

If PREDICATE is specified, only return functions for which
predicate returns true."
  (let (fs)
    (mapatoms (lambda (x)
                (when (and (fboundp x) (string-match-p regexp (symbol-name x))
                           (or (not predicate) (funcall predicate x)))
                  (push x fs))))
    fs))

;; TODO: Finish `maybe-with-sudo'.
;; (defun maybe-with-sudo (f &rest args)
;;   "If the command is prefixed with sudo, use tramp to sudo.

;; This is because `shell-command' and friends can't ask for sudo
;; privileges on their own since they do not have any way to prompt
;; the user for input.

;; NOTE: Will barf if you supply switches to sudo."
;;   (let* ((i ())
;;          (command (stringp (car args)) (car args) (cadr args)))
;;     (if (and (not (string-match-p "sudo:" default-directory))
;;              (string-match "^sudo " command))
;;         (progn (setq command (substring command 5))
;;                (unwind-protect
;;                    (progn (sudo-toggle)
;;                           (apply f command args))
;;                  (sudo-toggle)))
;;       (apply f command args))))

;; (seq-doseq (c (filter-functions "shell-command"
;;                                 (lambda (f) (commandp (symbol-function f)))))
;;   (advice-add c :around #'maybe-with-sudo))

(use-package vterm
  :git "https://github.com/akermu/emacs-libvterm"
  :init
  (defun vterm--rename-buffer-as-title (title)
    (rename-buffer (format "*VTerm %s*" title) t))

  (defun vterm--set-background-color ()
    (make-local-variable 'ansi-color-names-vector)
    (aset ansi-color-names-vector 0
          (plist-get (face-spec-choose (fiat-theme-face 'default)) :background)))

  (defun maybe-enable-hl-line-mode ()
    (if (eq major-mode 'vterm-mode)
        (global-hl-line-mode -1)
      (global-hl-line-mode +1)))

  (defvar vterm-install t "Tell `vterm' to compile if necessary.")
  :config
  (add-to-list 'vterm-set-title-functions #'vterm--rename-buffer-as-title)
  :hook
  (vterm-mode-hook . vterm--set-background-color)
  (window-configuration-change-hook . maybe-enable-hl-line-mode)
  :bind
  ("s-t" . vterm)
  ("C-c t" . vterm)
  ("s-T" . vterm-other-window)
  ("C-c T" . vterm-other-window))

(use-package term
  :bind
  (:map term-mode-map
        ("M-p" . term-send-up)
        ("M-n" . term-send-down)
        :map term-raw-map
        ("M-o" . other-window)
        ("M-p" . term-send-up)
        ("M-n" . term-send-down)
        ("C-M-j" . term-switch-to-shell-mode)))

(use-package xterm-color
  :commands
  xterm-color-colorize-buffer
  xterm-color-filter
  :config
  (defun xterm-color-shell-command (&rest _rest)
    "Colorize the output of `shell-command'."
    (dolist (b (buffer-list))
      (pcase (buffer-name b)
        ("*Shell Command Output*"
         (with-current-buffer b (xterm-color-colorize-buffer)))
        (" *Echo Area 0*"
         (with-current-buffer b (xterm-color-colorize-buffer))))))

  (defun xterm-color-apply-on-compile (proc)
    "Apply xterm color filtering on the `compilation-mode' running PROC."
    ;; We need to differentiate between compilation-mode buffers
    ;; and running as part of comint (which at this point we assume
    ;; has been configured separately for xterm-color)
    (when (eq (process-filter proc) 'compilation-filter)
      ;; This is a process associated with a compilation-mode buffer.
      ;; We may call `xterm-color-filter' before its own filter function.
      (set-process-filter proc (lambda (proc string)
                                 (funcall 'compilation-filter proc
                                          (xterm-color-filter string))))))

  (defun xterm-color-shell-setup ()
    ;; Disable font-locking in this buffer to improve performance
    (font-lock-mode -1)
    ;; Prevent font-locking from being re-enabled in this buffer
    (make-local-variable 'font-lock-function)
    (setq font-lock-function (lambda (_) nil))
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))

  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (advice-add #'shell-command :after #'xterm-color-shell-command)
  (advice-add #'shell-command-on-region :after #'xterm-color-shell-command)

  (with-eval-after-load 'compile
    (eval-when-compile (defvar compilation-environment))
    (add-to-list 'compilation-environment "TERM=xterm-256color"))
  :hook
  (shell-mode-hook . xterm-color-shell-setup)
  (compilation-start-hook . xterm-color-apply-on-compile))

(defun fpw (command)
  "Run `fpw' command as COMMAND.

Copy the result to the `kill-ring'. Call with a prefix argument
to modify the args."
  (interactive (list (if current-prefix-arg
                         (read-shell-command "Run fpw (like this): "
                                             "fpw " 'fpw-history)
                       "fpw")))
  (let ((result
         (replace-regexp-in-string "\n" "" (shell-command-to-string command))))
    (kill-new result)
    (message result)))

(bind-keys
 ("C-c C-v" . expand-environment-variable)
 ("C-:" . tramp-insert-remote-part)
 ("M-m p" . fpw)
 :map m-toggle-map
 ("s" . sudo-toggle))


;;;; Eshell

(use-package eshell
  :custom
  (eshell-banner-message "")
  (eshell-buffer-shorthand t)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-error-if-no-glob t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  (eshell-prompt-function 'm-eshell-prompt-function)
  (eshell-prompt-regexp "^(#?) ")
  (eshell-highlight-prompt nil)
  (eshell-ls-clutter-regexp (regexp-opt '(".cache" ".DS_Store" ".Trash" ".lock"
                                          "_history" "-history" ".tmp" "~"
                                          "desktop.ini" "Icon\r" "Thumbs.db"
                                          "$RECYCLE_BIN" "lost+found")))
  :commands
  eshell
  eshell-previous-input
  eshell-previous-prompt
  eshell-next-prompt
  eshell-ls-applicable
  eshell/cd
  eshell/pwd
  :config
  (defun eshell-prompt-housekeeping ()
    "Housekeeping for Eshell prompt."
    (setq xterm-color-preserve-properties t)
    (rename-buffer (format "*Eshell: %s*" default-directory) t))

  (defun eshell-other-window (arg)
    "Opens an eshell in another window, creating a new one if ARG is specified."
    (interactive "p")
    (if (= arg 4)
        (let* ((parent (if (buffer-file-name)
                           (file-name-directory (buffer-file-name))
                         default-directory))
               ;; `eshell' uses this variable as the new buffer name
               (eshell-buffer-name (concat "*eshell: "
                                           (car (last (split-string parent "/" t)))
                                           "*")))
          (switch-to-buffer-other-window "*eshell-here-temp*")
          (eshell)
          (kill-buffer "*eshell-here-temp*")
          (insert (concat "ls"))
          (eshell-queue-input))
      (progn
        (switch-to-buffer-other-window "*eshell*")
        (eshell)
        (message (number-to-string arg)))))

  (defun eshell-create-send (cmd &optional name)
    "Create an eshell buffer named NAME and run CMD in it."
    (let ((eshell-buffer-name (or name cmd)))
      (eshell))
    (insert cmd)
    (eshell-queue-input))

  (defun eshell-vertical-create-send (cmd &optional name)
    "Create an eshell buffer named NAME and run CMD in it.
Split the window vertically."
    (split-window-vertically)
    (eshell-create-send cmd name))

  (defun eshell-create-in-background ()
    "Create a new Eshell buffer but don't display it."
    (let ((eshell-buffer-name (generate-new-buffer "*Eshell*")))
      (save-window-excursion (eshell))))

  (defun eshell-get-or-create ()
    "Get or create an Eshell buffer."
    (interactive)
    (or (when current-prefix-arg (eshell-create-in-background))
        (car (filter-buffers-by-mode 'eshell-mode))
        (eshell-create-in-background)))

  (defun eshell-switch-to-buffer ()
    "Switch to the most recent Eshell buffer or create a new one.)))
This is different than the normal `eshell' command in my setup
because I dynamically rename the buffer according to
`default-directory'."
    (interactive)
    (switch-to-buffer (eshell-get-or-create)))

  (defun eshell-switch-to-buffer-other-window ()
    "Get or create an Eshell buffer, then switch to it."
    (interactive)
    (switch-to-buffer-other-window (eshell-get-or-create)))

  (defun switch-to-eshell-buffer ()
    "Interactively choose an Eshell buffer."
    (interactive)
    (switch-to-buffer-by-mode 'eshell-mode))

  (defun eshell/s (host)
    "Change directory to HOST via tramp."
    (eshell/cd (concat "/ssh:" host ":")))

  (defun eshell/e (&optional path)
    "Eshell alias for `find-file', passing optional PATH."
    (find-file path))

  (defun eshell/ee (&optional path)
    "Eshell alias for `find-file-other-window', passing optional PATH."
    (find-file-other-window path))

  (defun eshell/d (&optional path)
    "Eshell alias for `dired', passing optional PATH."
    (dired path))

  (defun eshell/do (&optional path)
    "Eshell alias for `dired-other-window', passing optional PATH."
    (dired-other-window path))

  (defun eshell-maybe-bol ()
    "Smarter `beginning-of-line' for Eshell."
    (interactive)
    (let ((p (point)))
      (eshell-bol)
      (if (= p (point))
          (beginning-of-line))))

  (defun eshell-quit-or-delete-char (arg)
    "Quit Eshell if `C-d' is specified, passing ARG on."
    (interactive "p")
    (defvar eshell-prompt-regexp)
    (if (and (eolp) (looking-back eshell-prompt-regexp nil))
        (progn
          (eshell-life-is-too-much)
          (ignore-errors
            (delete-window)))
      (delete-char  arg)))

  (defun eshell-send-previous-input (&optional arg)
    "Re-run the previous command with ARG in the last used eshell buffer."
    (interactive "*p")
    (require 'em-hist)
    (require 'esh-mode)
    (with-current-buffer
        (cl-some (lambda (b) (eq 'eshell-mode (with-current-buffer b major-mode)))
                 (buffer-list))
      (with-selected-window (get-buffer-window)
        (goto-char (point-max))
        (recenter 4))
      (eshell-previous-input arg)
      (eshell-send-input)))

  (defun eshell-send-current-line ()
    "Insert text of current line in eshell and execute."
    (interactive)
    (require 'eshell)
    (let ((command (buffer-substring
                    (save-excursion
                      (beginning-of-line)
                      (point))
                    (save-excursion
                      (end-of-line)
                      (point)))))
      (let ((buf (current-buffer)))
        (unless (get-buffer eshell-buffer-name)
          (eshell))
        (display-buffer eshell-buffer-name t)
        (switch-to-buffer-other-window eshell-buffer-name)
        (goto-char (point-max))
        (eshell-kill-input)
        (insert command)
        (eshell-send-input)
        (goto-char (point-max))
        (switch-to-buffer-other-window buf))))

  (defun m-eshell-prompt-function ()
    "Produce a highlighted prompt for Eshell."
    (mapconcat
     (lambda (list)
       (when list
         (propertize (concat " " (car list) " ")
                     'read-only t
                     'font-lock-face (cdr list)
                     'front-sticky '(font-lock-face read-only)
                     'rear-nonsticky '(font-lock-face read-only))))
     `(,(unless (eshell-exit-success-p)
          `(,(number-to-string eshell-last-command-status)
            :background "red" :foreground "white" :weight bold))
       (,(abbreviate-file-name (eshell/pwd)) :background "cyan" :foreground "black")
       (,(if (zerop (user-uid)) "\n(#)" "\n()")
        :foreground ,(if (equal 'light (frame-parameter nil 'background-mode))
                         "black"
                       "white")
        :weight bold))
     ""))

  (defun tramp-colon-prefix-expand (path)
    "Expand a colon prefix in PATH with the TRAMP remove prefix.

Examples:
  > cd /etc -> /etc
  > cd :/etc -> /sshx:host:/etc
  > cd : -> /sshx:host:/home/user"
    (if (file-remote-p default-directory)
        (cond
         ((string-prefix-p ":" path)
          (concat (file-remote-p default-directory)
                  (substring path 1)))
         (t path))
      path))

  (defun tramp-colon-prefix-maybe-expand ()
    "If we just inserted a colon prefix, run `tramp-colon-prefix-expand' on it."
    (when (looking-back "\\_<:" nil)
      (delete-char -1)
      (insert (tramp-colon-prefix-expand (concat ":" (thing-at-point 'filename))))))

  ;; Advise `eshell/*' functions to work with `:' prefix path syntax.
  (seq-doseq (c '(cd cp mv rm e ee d do))
    (advice-add (intern (concat "eshell/" (symbol-name c)))
                :filter-args
                (lambda (args) (mapcar #'tramp-colon-prefix-expand args))))

  (defun eshell/really-clear ()
    "Call `eshell/clear' with an argument to really clear the buffer.
Call it a second time to print the prompt."
    (interactive)
    (eshell/clear 1)
    (eshell/clear))

  (defun eshell/info (&rest args)
    "Invoke `info', optionally opening the Info system to car ARGS."
    (Info-directory)
    (let ((subject (car args)))
      (if (not (null subject))
          (let ((node-exists (ignore-errors (Info-menu subject))))
            (if (not node-exists)
                (format "No menu item `%s' in node `(dir)Top'." subject))))))

  (defun eshell-kill-previous-output ()
    "Kill the output of the previous command."
    (interactive)
    (let ((inhibit-read-only t)
          (lines (count-lines (eshell-beginning-of-output)
                              (eshell-end-of-output))))
      ;; Kill region
      (kill-region (eshell-beginning-of-output) (eshell-end-of-output))
      (save-excursion
        ;; Write something in place of the text so we know what happened.
        (goto-char (eshell-beginning-of-output))
        (insert (format "--- Killed %d lines ---\n" lines)))))

  (defun eshell-kill-previous-output-to-buffer ()
    "Move output of the previous command to a new buffer."
    (interactive)
    (eshell-kill-previous-output)
    (switch-to-buffer-other-window "*eshell-stdout*")
    (yank))

  (defun eshell-copy-previous-output ()
    "Copies the output of the previous command to the kill ring."
    (interactive)
    (let ((lines (count-lines (eshell-beginning-of-output)
                              (eshell-end-of-output))))
      ;; Copy region to kill ring
      (copy-region-as-kill (eshell-beginning-of-output) (eshell-end-of-output))
      (message "Copied %d lines" lines)))

  (defun eshell/import-aliases ()
    "Retrieve bash aliases and format them for import into Eshell."
    (shell-command ". ~/.env && . ~/.aliases && alias | sed -E \"s/^alias ([^=]+)='(.*)'$/alias \\1 \\2 \\$*/g; s/'\\\\''/'/g;\""
                   "*bash aliases*"))

  ;; Redefine `eshell-exec-visual' to by TRAMP aware for ssh
  ;; https://gist.github.com/ralt/a36288cd748ce185b26237e6b85b27bb
  (defvar eshell-term-name)
  (defvar eshell-parent-buffer)
  (defvar eshell-escape-control-x)

  (defun eshell-exec-visual (&rest args)
    "Run the specified PROGRAM in a terminal emulation buffer.
 ARGS are passed to the program.  At the moment, no piping of input is
 allowed."
    (let* (eshell-interpreter-alist
           (original-args args)
           (interp (eshell-find-interpreter (car args) (cdr args)))
           (in-ssh-tramp (and (tramp-tramp-file-p default-directory)
                              (equal (tramp-file-name-method
                                      (tramp-dissect-file-name default-directory))
                                     "ssh")))
           (program (if in-ssh-tramp
                        "ssh"
                      (car interp)))
           (args (if in-ssh-tramp
                     (let ((dir-name (tramp-dissect-file-name default-directory)))
                       (flatten-tree
                        (list
                         "-t"
                         (tramp-file-name-host dir-name)
                         (format
                          "export TERM=xterm-256color; cd %s; exec %s"
                          (tramp-file-name-localname dir-name)
                          (string-join
                           (append
                            (list (tramp-file-name-localname (tramp-dissect-file-name (car interp))))
                            (cdr args))
                           " ")))))
                   (flatten-tree
                    (eshell-stringify-list (append (cdr interp)
                                                   (cdr args))))))
           (term-buf
            (generate-new-buffer
             (concat "*"
                     (if in-ssh-tramp
                         (format "%s %s" default-directory (string-join original-args " "))
                       (file-name-nondirectory program))
                     "*")))
           (eshell-buf (current-buffer)))
      (save-current-buffer
        (switch-to-buffer term-buf)
        (term-mode)
        (set (make-local-variable 'term-term-name) eshell-term-name)
        (make-local-variable 'eshell-parent-buffer)
        (setq eshell-parent-buffer eshell-buf)
        (term-exec term-buf program program nil args)
        (let ((proc (get-buffer-process term-buf)))
          (if (and proc (eq 'run (process-status proc)))
              (set-process-sentinel proc 'eshell-term-sentinel)
            (error "Failed to invoke visual command")))
        (term-char-mode)
        (if eshell-escape-control-x
            (term-set-escape-char ?\C-x))))
    nil)

  (defun eshell/init ()
    "Initialize the Eshell environment."
    (require 'em-term)
    (source-sh "~/.env")
    (setq eshell-path-env (getenv "PATH"))
    ;; Path to shell executable. Set it this way to work with tramp.
    (setenv "SHELL" "/bin/bash")
    ;; (setenv "TERM" "eterm-color")
    (setenv "EDITOR" "emacsclient")
    (setenv "PAGER" "cat")
    (setenv "MANPAGER" "cat")

    ;; xterm colors
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

    (defvar eshell-visual-commands)
    (add-to-list 'eshell-visual-commands "n")
    (advice-add 'eshell-ls-decorated-name :around #'m-eshell-ls-decorated-name)

    ;; Load the Eshell versions of `su' and `sudo'
    (require 'em-tramp)
    (add-to-list 'eshell-modules-list 'eshell-tramp)

    ;; Set up `tramp-colon-prefix'.
    (add-hook 'post-self-insert-hook #'tramp-colon-prefix-maybe-expand nil t)

    (bind-keys
     :map eshell-mode-map
     ("C-a" . eshell-maybe-bol)
     ("C-d" . eshell-quit-or-delete-char)
     ("<tab>" . completion-at-point)
     ("M-r" . counsel-esh-history)
     ("C-L" . eshell/really-clear)
     ("C-w" . eshell-kill-previous-output)
     ("C-M-w" . eshell-kill-previous-output-to-buffer)
     ("M-w" . eshell-copy-previous-output)
     ("s-v" . clipboard-yank)
     ("C-S-<backspace>" . eshell-kill-input)
     ("C-M-S-p" . eshell-previous-prompt)
     ("M-<up>" . eshell-previous-prompt)
     ("C-M-S-n" . eshell-next-prompt)
     ("M-<down>" . eshell-next-prompt)
     ("C-h C-e" . esh-help-run-help)))

  (defun eshell-ls-find-file-at-point ()
    "RET on Eshell's `ls' output to open files."
    (interactive)
    (find-file (substring-no-properties (thing-at-point 'filename))))

  (defvar m-eshell-ls-file-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "RET") 'eshell-ls-find-file-at-point)
      (define-key map (kbd "<return>") 'eshell-ls-find-file-at-point)
      (define-key map [mouse-1] 'eshell-ls-find-file-at-point)
      map)
    "Keys in effect when point is over a file from `eshell/ls'.")

  (defun m-eshell-ls-decorated-name (f &rest args)
    "Call F with ARGS.

Add more decoration to files in `eshell/ls' output.

* Mark directories with a `/'
* Mark execurables with a `*'
* Open files and directories with `RET' or `mouse-1'

Advise `eshell-ls-decorated-name'."
    (let* ((file (car args))
           (name (apply f args))
           (suffix
            (cond
             ;; Directory
             ((eq (cadr file) t)
              "/")
             ;; Executable
             ((and (/= (user-uid) 0)            ; root can execute anything
                   (eshell-ls-applicable (cdr file) 3 'file-executable-p (car file)))
              "*"))))
      (propertize
       (if (and suffix (not (string-suffix-p suffix name)))
           (concat name suffix)
         name)
       'keymap m-eshell-ls-file-keymap
       'mouse-face 'highlight)))

  (defun ibuffer-show-eshell-buffers ()
    "Open an `ibuffer' window and display all Eshell buffers."
    (interactive)
    (require 'ibuffer)
    (ibuffer nil "Eshell Buffers" '((mode . eshell-mode)) nil t nil
             '(((name 64 64 :left) " " (process 0 -1 :right)))))

  :hook
  (eshell-mode-hook . eshell/init)
  (eshell-before-prompt-hook . eshell-prompt-housekeeping)

  :bind
  (("s-e" . eshell-switch-to-buffer)
   ("C-c e" . eshell-switch-to-buffer)
   ("s-E" . eshell-switch-to-buffer-other-window)
   ("C-c E" . eshell-switch-to-buffer-other-window)
   ("C-s-e" . switch-to-eshell-buffer)
   ("M-E" . ibuffer-show-eshell-buffers)
   ("C-c M-e" . ibuffer-show-eshell-buffers)
   :map prog-mode-map
   ("M-P" . eshell-send-previous-input)))

;; ElDoc and topical help in Eshell.
(use-package esh-help
  :config
  (setup-esh-help-eldoc)
  :commands
  (setup-esh-help-eldoc esh-help-run-help))

;; Fish-like autosuggestions.
(use-package esh-autosuggest
  :hook
  (eshell-mode-hook . esh-autosuggest-mode)
  (esh-autosuggest-mode-hook . (lambda ()
                                 (bind-key "C-e"
                                           #'company-complete-selection
                                           esh-autosuggest-active-map))))

(use-package eshell-bookmark
  :hook
  (eshell-mode-hook . eshell-bookmark-setup))


;;;; Lisp

;; Lisp specific functionality

(defun advice-remove-all (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(use-package srefactor
  :commands
  srefactor-lisp-format-buffer
  :config
  (require 'srefactor-lisp)

  (defun srefactor-lisp-format-region (beg end)
    "Format the region between BEG and END."
    (interactive "r")
    (narrow-to-region beg end)
    (srefactor-lisp-format-buffer)
    (widen))

  :hook
  ((c-mode-hook c++-mode-hook emacs-lisp-mode-hook) . semantic-mode)
  
  :bind
  ("C-c s RET" . srefactor-refactor-at-point))

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

(defun eval-last-sexp-other-window (arg)
  "Run `eval-last-sexp' with ARG in the other window."
  (interactive "P")
  (save-window-excursion (other-window 1)
                         (eval-last-sexp arg)))

(defun expression-to-register (register)
  "Interactively store an Emacs Lisp expression in a REGISTER.
If region is active, store that. Otherwise, store the sexp at
  point."
  (interactive (list (register-read-with-preview "Copy expression to register: ")))
  (set-register register
                (if (region-active-p)
                    (buffer-substring (mark) (point))
                  (cl-destructuring-bind
                      (start . end) (bounds-of-thing-at-point 'sexp)
                    (buffer-substring start end))))
  (setq deactivate-mark t)
  (when (called-interactively-p 'interactive) (indicate-copied-region)))

(defun eval-register (register)
  "Evaluate contents of register REGISTER as an Emacs Lisp expression.
REGISTER is a character and its contents are a string.

If called with a prefix arg, then insert the return value at
point.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (register-read-with-preview "Eval register: ")
                       current-prefix-arg)))
  (let* ((val (get-register register))
         (res (eval (car (read-from-string (format "(progn %s)" val))))))
    (when current-prefix-arg (register-val-insert res))))

(use-package clojure-mode
  :mode
  (("\\.clj\\'" . clojure-mode)
   ("\\.cljs\\'" . clojurescript-mode)
   ("\\.cljc\\'" . clojurec-mode))
  :interpreter
  ("inlein" . clojure-mode))

(use-package clojure-mode-extra-font-locking
  :after clojure-mode)

(use-package inf-clojure
  :commands
  inf-clojure
  inf-clojure-connect
  inf-clojure-minor-mode
  :config
  (defun inf-clojure-start-lumo ()
    "Start lumo as a subprocess and then connect to it over TCP.
This is preferable to starting it directly because lumo has lots
of problems in that context."
    (interactive)
    (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
    (inf-clojure-minor-mode)
    (shell-command "pkill -f 'lumo -d -n 2000'")
    (async-shell-command "lumo -d -n 2000")
    (run-with-idle-timer 2 nil (lambda () (inf-clojure-connect "localhost" 2000))))
  :bind
  (:map inf-clojure-minor-mode-map
        ("s-<return>" . inf-clojure-eval-last-sexp)
        ("C-c C-k" . inf-clojure-eval-buffer)))

(use-package cider
  :config
  (defun toggle-nrepl-buffer ()
    "Toggle the nREPL REPL on and off."
    (interactive)
    (if (string-match "cider-repl" (buffer-name (current-buffer)))
        (delete-window)
      (cider-switch-to-repl-buffer)))

  (defun cider-save-and-refresh ()
    "Save the buffer and refresh CIDER."
    (interactive)
    (save-buffer)
    (call-interactively 'cider-refresh))

  (defun cider-eval-last-sexp-and-append ()
    "Eval last sexp and append the result."
    (interactive)
    (cider-eval-last-sexp '(1)))

  :custom
  ;; Always prompt for the jack in command.
  (cider-edit-jack-in-command t)

  :commands
  (cider-jack-in cider-switch-to-repl-buffer)

  :hook
  ;; The standard advice function runs at the wrong time I guess? Anyway, it
  ;; often gets set to the wrong color when switching themes via `theme-choose'.
  (theme . (lambda () (when (fboundp 'cider-scale-background-color)
                        (setq cider-stacktrace-frames-background-color
                              (cider-scale-background-color)))))
  :bind
  (:map cider-mode-map
        ("s-<return>" . cider-eval-last-sexp)))

(use-package sly
  :custom
  (inferior-lisp-program (executable-find "sbcl"))
  :bind
  (:map sly-prefix-map
        ("M-h" . sly-documentation-lookup)))

(use-package scheme
  :mode ("\\.scheme\\'" . scheme-mode)
  :commands
  scheme-syntax-propertize-sexp-comment
  :config
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))

  (defun scheme-region-extend-function ()
    (when (not (get-text-property (point) 'font-lock-multiline))
      (let* ((heredoc nil)
             (new-beg
              (save-excursion
                (when (and (re-search-backward "#>\\|<#\\|#<[<#]\\(.*\\)$" nil t)
                           (not (get-text-property (point) 'font-lock-multiline)))
                  (let ((match (match-string 0))
                        (tag (match-string 1)))
                    (cond
                     ((equal match "#>") (point))
                     ((string-match-p "^#<[<#]" match) (setq heredoc tag) (point)))))))
             (new-end
              (save-excursion
                (if heredoc
                    (when (and (re-search-forward (concat "^" (regexp-quote heredoc) "$") nil t)
                               (not (get-text-property (point) 'font-lock-multiline)))
                      (point))
                  (when (and (re-search-forward "#>\\|<#" nil t)
                             (not (get-text-property (point) 'font-lock-multiline))
                             (equal (match-string 0) "<#"))
                    (point))))))
        (when (and new-beg new-end)
          (setq font-lock-beg new-beg)
          (setq font-lock-end new-end)
          (with-silent-modifications
            (put-text-property new-beg new-end 'font-lock-multiline t))
          (cons new-beg new-end)))))

  (defun scheme-syntax-propertize-foreign (_beg end)
    (save-match-data
      (when (search-forward "<#" end t)
        (with-silent-modifications
          (put-text-property (1- (point)) (point)
                             'syntax-table (string-to-syntax "> cn"))))))

  (defun scheme-syntax-propertize-heredoc (_beg _end)
    (save-match-data
      (let ((tag (match-string 2)))
        (when (and tag (re-search-forward (concat "^" (regexp-quote tag) "$") nil t))
          (with-silent-modifications
            (put-text-property (1- (point)) (point)
                               'syntax-table (string-to-syntax "> cn")))))))

  (defun scheme-syntax-propertize (beg end)
    (goto-char beg)
    (scheme-syntax-propertize-sexp-comment (point) end)
    (funcall
     (syntax-propertize-rules
      ("\\(#\\);"
       (1 (prog1 "< cn" (scheme-syntax-propertize-sexp-comment (point) end))))
      ("\\(#\\)>"
       (1 (prog1 "< cn" (scheme-syntax-propertize-foreign (point) end))))
      ("\\(#\\)<[<#]\\(.*\\)$"
       (1 (prog1 "< cn" (scheme-syntax-propertize-heredoc (point) end)))))
     (point) end))

  (defun scheme-mode-setup ()
    "Set up `scheme-mode'."

    (add-hook 'scheme-mode-hook
              (lambda ()
                (setq font-lock-extend-region-functions
                      (cons 'scheme-region-extend-function
                            font-lock-extend-region-functions)))))
  :hook
  (scheme-mode-hook . scheme-mode-setup))

(use-package geiser
  :custom
  (geiser-default-implementation 'chicken)
  (geiser-mode-eval-last-sexp-to-buffer t)
  (scheme-program-name "csi -:c")
  :config
  ;; Indenting module body code at column 0
  (defun scheme-module-indent (_state _indent-point _normal-indent) 0)
  (put 'module 'scheme-indent-function 'scheme-module-indent)
  (put 'and-let* 'scheme-indent-function 1)
  (put 'parameterize 'scheme-indent-function 1)
  (put 'handle-exceptions 'scheme-indent-function 1)
  (put 'when 'scheme-indent-function 1)
  (put 'unless 'scheme-indenfunction 1)
  (put 'match 'scheme-indent-function 1)
  :commands
  (geiser run-geiser run-chicken))

(bind-keys
 :map lisp-mode-shared-map
 ("s-<return>" . eval-last-sexp)
 ("C-s-<return>" . eval-last-sexp-other-window)
 ("C-c C-k" . eval-buffer)
 ("C-x C-r" . eval-region)
 ("C-x M-e" . pp-macroexpand-last-sexp)
 ("C-x r E" . expression-to-register)
 ("C-x r e" . eval-register))


;;;; Log Files

(use-package vlf
  :defer 5
  :custom
  (vlf-application 'dont-ask)
  :config
  (require 'vlf-setup))

;; Dependency for `logview'.
(use-package datetime
  :custom
  (datetime-timezone 'US/Pacific))

(use-package logview
  :mode "\\.log.*"
  :custom
  (logview-additional-timestamp-formats
   '(("ISO 8601 datetime (with 'T' and 'Z') + millis"
      (java-pattern . "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"))
     ("millis"
      (java-pattern . "SSSSS"))))
  (logview-additional-level-mappings
   '(("VMware" . ((error "error")
                  (warning "warning")
                  (information "info")
                  (debug "debug")
                  (trace "trace")))
     ("NPM" . ((error "error")
               (warning "warn")
               (information "info")
               (debug "verbose")
               (trace "silly")))))
  (logview-additional-submodes
   '(("VMware" . ((format  . "TIMESTAMP LEVEL NAME [THREAD] ")
                  (levels  . "VMware")))
     ("NPM" . ((format . "TIMESTAMP LEVEL NAME THREAD")
               (levels . "NPM"))))))


;;;; Docker

(use-package dockerfile-mode
  :mode "\\`Dockerfile")

(use-package docker
  :bind
  ("C-c M-d" . docker))

(use-package docker-tramp
  :after tramp
  :defer 5)

;; dw (https://gitlab.com/mnewt/dw)
(add-to-list 'auto-mode-alist '("DWfile\\'" . sh-mode))
(add-to-list 'interpreter-mode-alist '("dw" . sh-mode))

;;;; Web

(use-package eww
  :config
  (use-package shr-tag-pre-highlight
    :after shr
    :hook
    (eww-mode-hook . (lambda () (add-to-list 'shr-external-rendering-functions
                                             '(pre . shr-tag-pre-highlight))))
    :commands
    (shr-tag-pre-highlight))
  :commands
  eww)

(use-package w3m
  :custom
  (w3m-search-engine-alist
   '(("google" "https://www.google.com/search?q=%s&ie=utf-8&oe=utf-8&gbv=1" utf-8)
     ("emacswiki" "https://www.emacswiki.org/cgi-bin/wiki?search=%s")
     ("en.wikipedia" "https://en.wikipedia.org/wiki/Special:Search?search=%s")
     ("duckduckgo" "https://duckduckgo.com/lite&q=%s" utf-8)))
  (w3m-search-default-engine "duckduckgo")
  :commands
  (w3m w3m-goto-url w3m-search))

(use-package markdown-mode
  :mode "\\.md\\|markdown\\'"
  :custom
  (markdown-list-indent-width tab-width)
  (markdown-command "multimarkdown"))

(use-package web-mode
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'")
  :init
  ;; from web-mode FAQ to work with smartparens
  (defun web-mode-setup ()
    (setq web-mode-enable-auto-pairing nil))

  (defun sp-web-mode-is-code-context (_id action _context)
    (and (eq action 'insert)
         (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))
  :custom
  (sgml-basic-offset tab-width)
  (web-mode-markup-indent-offset tab-width)
  (web-mode-css-indent-offset tab-width)
  (web-mode-code-indent-offset tab-width)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-ac-sources-alist
   '(("css" . (ac-source-css-property))
     ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  :config
  (use-package company-web
    :commands
    (company-web-html)
    :hook
    (web-mode-hook . (lambda () (set (make-local-variable 'company-backends)
                                     (cons 'company-web-html company-backends)))))
  :hook
  (web-mode-hook . web-mode-setup))

(use-package css-mode
  :mode "\\.css\\'"
  :custom
  (css-indent-offset tab-width))

(use-package sass-mode
  :ensure-system-package
  (sass . "gem install sass")
  :mode "\\(?:s\\(?:[ac]?ss\\)\\)")

(use-package restclient
  :mode ("\\.restclient\\'" . restclient-mode)
  :config
  (use-package company-restclient
    :hook
    (restclient-mode-hook . (lambda ()
                              (add-to-list 'company-backends 'company-restclient))))

  (use-package know-your-http-well
    :commands
    (http-header http-method http-relation http-status-code))

  :commands
  (restclient-mode restclient-outline-mode))

;;;; Javascript

(use-package add-node-modules-path
  :hook
  ((css-mode-hook
    graphql-mode-hook
    js2-mode-hook
    markdown-mode-hook
    sass-mode-hook
    web-mode-hook) . add-node-modules-path))

(use-package js
  :mode ("\\.jsx?\\'" . js-mode)
  :custom
  (js-indent-level tab-width))

(use-package json-mode
  :ensure-system-package jq
  :mode "\\.json\\|prettierrc\\'")
;; :hook
;; (json-mode-hook . (lambda () (prettier-babel-on-save-mode -1))))

(use-package graphql-mode
  :mode "\\(?:\\.g\\(?:\\(?:raph\\)?ql\\)\\)\\'")

;;;; Python

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3?" . python-mode)
  :custom
  (gud-pdb-command-name "python -m pdb")
  :bind
  (:map python-mode-map
        ("s-v" . yank)
        ("s-<return>" . python-shell-send-defun)))

(use-package lsp-python-ms
  :after lsp-mode
  :hook
  (python-mode-hook . lsp-deferred))

;;;; Applescript

(use-package apples-mode
  :mode "\\.\\(applescri\\|sc\\)pt\\'"
  :commands
  apples-open-scratch)

;; Allow editing of binary .scpt files (applescript) on mac.
;; https://www.emacswiki.org/emacs/AppleScript
(add-to-list 'jka-compr-compression-info-list
             `["\\.scpt\\'"
               "converting text applescript to binary applescript "
               ,(expand-file-name "applescript-helper" "~/.emacs.d/bin/") nil
               "converting binary applescript to text applescprit "
               ,(expand-file-name "applescript-helper" "~/.emacs.d/bin/") ("-d")
               nil t "FasdUAS"])
;;It is necessary to perform an update!
(jka-compr-update)

;;;; File Modes

;; All modes and mode related stuff which doesn't fit into a larger category.

(use-package flyspell
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra"))
  :commands
  flyspell-mode flyspell-prog-mode
  :hook
  (text-mode-hook . flyspell-mode)
  (prog-mode-hook . flyspell-prog-mode)
  :bind
  (:map flyspell-mode-map
        ("C-," . nil)
        ("C-." . nil)
        ("C-;" . nil)
        ("C-M-i" . nil)))

(use-package flyspell-correct-ivy
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ([remap flyspell-correct-word-before-point] .
         flyspell-correct-previous-word-generic)))

(use-package flycheck
  :defer 7
  :custom
  (flycheck-check-syntax-automatically '(idle-change idle-buffer-switch))
  (flycheck-idle-change-delay 1)
  (flycheck-idle-buffer-switch-delay 1)
  (flycheck-global-modes '(not lisp-interaction-mode))
  (flycheck-mode-line-prefix "")
  :commands
  flycheck-define-error-level
  flycheck-list-errors
  flycheck-error-list-set-filter
  flycheck-next-error
  flycheck-previous-error
  flycheck-first-error
  :config
  ;; Stolen from spacemacs
  (define-fringe-bitmap 'my-flycheck-fringe-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))

  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)

  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info)

  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-hint-display-type t) (flycheck-list-errors))
          :post (progn (setq hydra-hint-display-type nil)
                       (quit-windows-on "*Flycheck errors*"))
          :hint nil)
    "Errors"
    ("f" flycheck-error-list-set-filter "Filter")
    ("n" flycheck-next-error "Next")
    ("p" flycheck-previous-error "Previous")
    ("<" flycheck-first-error "First")
    (">" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q" nil))

  (global-flycheck-mode)

  :bind
  (("C-c ! !" . flycheck-mode)
   :map flycheck-mode-map
   ("C-c ! ." . hydra-flycheck/body)))

(use-package lsp-mode
  :custom
  (lsp-enable-snippet t)
  (lsp-auto-guess-root t)
  (lsp-eldoc-render-all t)
  (lsp-prefer-flymake nil)
  (lsp-before-save-edits t)
  :commands
  (lsp lsp-deferred)
  :hook
  ((c-mode-hook c++-mode-hook css-mode-hook go-mode-hook
                java-mode-hook js-mode-hook php-mode-hook
                powershell-mode-hook enh-ruby-mode-hook
                nxml-mode-hook rust-mode-hook sass-mode-hook
                sh-mode-hook html-mode-hook web-mode-hook
                xml-mode-hook) . lsp-deferred)
  (lsp-after-open-hook . lsp-enable-imenu))

(use-package lsp-ui :commands lsp-ui-mode
  :bind
  (:map lsp-ui-mode-map
        ("M-." . lsp-ui-peek-find-definitions)
        ("M-?" . lsp-ui-peek-find-references)
        ("C-h ." . lsp-ui-doc-show)))

(use-package company-lsp :commands company-lsp)

;; TODO Test out `dap-mode'.
;; (use-package dap-mode)

(defgroup m-reformatter nil
  "Customized reformatter parts."
  :prefix "m-"
  :group 'tools)

(defvar m-reformatters nil
  "Alist mapping major mode to formatter commands.

KEY is a major mode symbol.

VALUE is a `reformatter' symbol which is either the symbol from a
`reformatter-define' statement (e.g. `zprint') or the symbol
referencing a format region function, which takes two arguments:
`beginning' and `end' (e.g. `format-region').")

(use-package reformatter
  :defer 7
  :commands
  reformatter-define
  :config

  (defvar m-clojure-command (executable-find "clojure"))
  (reformatter-define zprint
    :program m-clojure-command
    :args '("-A:zprint")
    :group 'm-reformatter)
  (add-to-list 'm-reformatters '(clojure-mode . zprint))
  (add-to-list 'm-reformatters '(clojurec-mode . zprint))
  (add-to-list 'm-reformatters '(clojurescript-mode . zprint))

  (defvar m-prettier-command (executable-find "prettier"))
  (reformatter-define prettier-babel
    :program m-prettier-command
    :args '("--parser" "babel")
    :group 'm-reformatter)
  (add-to-list 'm-reformatters '(js-mode . prettier-babel))
  (reformatter-define prettier-css
    :program m-prettier-command
    :args '("--parser" "css")
    :group 'm-reformatter)
  (add-to-list 'm-reformatters '(css-mode . prettier-css))
  (reformatter-define prettier-scss
    :program m-prettier-command
    :args '("--parser" "scss")
    :group 'm-reformatter)
  (add-to-list 'm-reformatters '(scss-mode . prettier-scss))
  (reformatter-define prettier-html
    :program m-prettier-command
    :args '("--parser" "html")
    :group 'm-reformatter)
  (add-to-list 'm-reformatters '(html-mode . prettier-html))
  (add-to-list 'm-reformatters '(web-mode . prettier-html))
  (reformatter-define prettier-graphql
    :program m-prettier-command
    :args '("--parser" "graphql")
    :group 'm-reformatter)
  (add-to-list 'm-reformatters '(graphql-mode . prettier-graphql))
  (reformatter-define prettier-markdown
    :program m-prettier-command
    :args '("--parser" "markdown")
    :group 'm-reformatter)
  (add-to-list 'm-reformatters '(markdown-mode . prettier-markdown))
  (reformatter-define prettier-yaml
    :program m-prettier-command
    :args '("--parser" "yaml")
    :group 'm-reformatter)
  (add-to-list 'm-reformatters '(yaml-mode . prettier-yaml))

  (defvar m-xmllint-command (executable-find "xmllint"))
  (reformatter-define xmllint
    :program m-xmllint-command
    :args '("--format" "-")
    :group 'm-reformatter)
  (add-to-list 'm-reformatters '(nxml-mode . xmllint))

  (defvar m-black-command (executable-find "black"))
  (reformatter-define black
    :program m-black-command
    :args '("-q" "--line-length" "80")
    :group 'm-reformatter)
  (add-to-list 'm-reformatters '(python-mode . black))

  (defvar m-shfmt-command (executable-find "shfmt"))
  (reformatter-define shfmt
    :program m-shfmt-command
    :group 'm-reformatter)
  (add-to-list 'm-reformatters '(sh-mode . shfmt))

  (cl-loop for (mode . sym) in m-reformatters do
           (add-hook (intern (concat (symbol-name mode) "-hook"))
                     (intern (concat (symbol-name sym) "-on-save-mode"))))

  (defun reformat-region (beg end)
    "Reformat the region.

This is a fallback in case we can't find a dedicated reformatter
for the buffer."
    (interactive)
    (indent-region beg end))

  (defun reformat-buffer ()
    "Reformat the buffer.

This is a fallback in case we can't find a dedicated reformatter
for the buffer."
    (interactive)
    (reformat-region (point-min) (point-max)))

  (defun reformat-buffer-or-region (beg end &optional thing)
    "Reformat the region from BEG to END.

If no region is active, format the buffer.

Prefix ARG is passed to `fill-paragraph'."
    (interactive "r")
    (when (sp-point-in-string-or-comment) (fill-paragraph current-prefix-arg))
    (call-interactively #'crux-cleanup-buffer-or-region)
    (let ((format-region-fn (let ((f (alist-get major-mode m-reformatters)))
                              (cl-some (lambda (x) (when (fboundp x) x))
                                       (list (intern (format "%s-region" f))
                                             f
                                             'reformat-region))))
          (beg (or beg (if (use-region-p) (region-beginning) (point-min))))
          (end (or end (if (use-region-p) (region-end) (point-max))))
          (thing (or thing (if (use-region-p) "region" "buffer"))))
      (funcall-interactively format-region-fn beg end)
      (message "Formatted the %s." thing)))

  (defun reformat-defun-or-region ()
    "Reformat the current defun or region."
    (interactive)
    (if (use-region-p)
        (reformat-buffer-or-region (region-beginning) (region-end) "region")
      (save-excursion
        (mark-defun)
        (reformat-buffer-or-region (region-beginning) (region-end) "defun"))))

  (defun reformat-line ()
    "Reformat the current line."
    (interactive)
    (reformat-buffer-or-region (line-beginning-position) (line-end-position) "line"))

  :bind
  ("C-M-\\" . reformat-buffer-or-region)
  ("C-\\" . reformat-defun-or-region))

(use-package sh-script
  :mode ("\\.sh\\'" . sh-mode)
  :interpreter ("sh" . sh-mode) ("bash" . sh-mode)
  :init
  (defun maybe-reset-major-mode ()
    "Reset the buffer's `major-mode' if a different mode seems like a better fit.
Mostly useful as a `before-save-hook', to guess mode when saving
a new file for the first time."
    (when (and (buffer-file-name)
               (not (file-exists-p (buffer-file-name)))
               (eq major-mode 'fundamental-mode))
      (normal-mode)))

  :custom
  (sh-basic-offset tab-width)
  (sh-indentation tab-width)
  ;; Tell `executable-set-magic' to insert #!/usr/bin/env interpreter
  (executable-prefix-env t)
  :config
  ;; Match variables in quotes. Fuco1 is awesome, mkay.
  ;; https://fuco1.github.io/2017-06-11-Font-locking-with-custom-matchers.html
  (defun shell-match-variables-in-quotes (limit)
    "Match variables in double-quotes in `sh-mode' with LIMIT."
    (with-syntax-table sh-mode-syntax-table
      (catch 'done
        (while (re-search-forward
                ;; `rx' is cool, mkay.
                (rx (or line-start (not (any "\\")))
                    (group "$")
                    (group
                     (or (and "{" (+? nonl) "}")
                         (and (+ (any alnum "_")))
                         (and (any "*" "@" "#" "?" "-" "$" "!" "0" "_")))))
                limit t)
          (-when-let (string-syntax (nth 3 (syntax-ppss)))
            (when (= string-syntax 34)
              (throw 'done (point))))))))

  (font-lock-add-keywords 'sh-mode '((shell-match-variables-in-quotes
                                      (1 'default t)
                                      (2 font-lock-variable-name-face t))))
  :hook
  (before-save-hook . maybe-reset-major-mode)
  (after-save-hook . executable-make-buffer-file-executable-if-script-p)
  :bind
  (:map sh-mode-map
        ("<return>" . newline-and-indent)
        ("RET" . newline-and-indent)
        ("C-c m" . executable-set-magic)
        ;; Don't shadow `yasnippet'.
        ("C-c C-s" . nil)
        ("C-c M-s" . sh-select)))


;; git config files
(add-to-list 'auto-mode-alist '("\\.git\\(?:config\\|ignore\\).*" . conf-mode))
;; SSH server config files
(add-to-list 'auto-mode-alist '("sshd\?_config" . conf-mode))
;; mbsync
(add-to-list 'auto-mode-alist '("\\.mbsyncrc" . conf-mode))

;; display nfo files in all their glory
;; https://github.com/wasamasa/dotemacs/blob/master/init.org#display-nfo-files-with-appropriate-code-page)
(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))

(use-package perl-mode
  :mode "\\.pl\\'"
  :custom
  (perl-indent-level tab-width))

(use-package fish-mode
  :mode "\\.fish\\'"
  :custom (fish-indent-offset tab-width))

(use-package systemd
  :mode
  ("\\.\\(?:automount\\|link\\|mount\\|net\\(?:dev\\|work\\)\\|path\\|s\\(?:ervice\\|lice\\|ocket\\)\\|t\\(?:arget\\|imer\\)\\)\\'" . systemd-mode))

;; DNS
(use-package dns-mode
  :mode "\\.rpz\\'")

;; (use-package genrnc
;;   :custom
;;   (genrnc-user-schemas-directory "~/.emacs.d/schema")
;;   :commands
;;   (genrnc-regist-file))

;; (use-package rnc-mode
;;   :mode "\\.rnc\\'")

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package nginx-mode
  :custom
  (nginx-indent-level tab-width)
  :commands
  (nginx-mode))

(use-package caddyfile-mode
  :mode "\\`Caddyfile.*")

(use-package yaml-mode
  :mode "\\.ya\?ml\\'")

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package enh-ruby-mode
  :ensure-system-package
  (rufo . "gem install rufo")
  :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
  :config
  (defvar inf-ruby-minor-mode-map)
  (use-package inf-ruby
    :hook
    (enh-ruby-mode-hook . inf-ruby-minor-mode)
    (compilation-filter . inf-ruby-auto-enter)
    :commands
    (inf-ruby inf-ruby-console-auto)
    :bind
    (:map inf-ruby-minor-mode-map
          ("s-<return>". ruby-send-last-sexp)
          ("C-M-x" . ruby-send-block))))

(use-package lua-mode
  :mode "\\.lua\\'"
  :custom
  (lua-indent-level tab-width))

(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-indent-offset tab-width))

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (use-package company-go
    :hook
    (go-mode-hook
     . (lambda () (set (make-local-variable 'company-backends) '(company-go))))))

(use-package powershell
  :mode "\\.ps1\\'"
  :custom
  (powershell-indent tab-width)
  (powershell-continuation-indent tab-width))

(use-package php-mode
  :mode "\\.php\\'")

(use-package IOS-config-mode
  :git "https://github.com/nibrahim/IOS-config-mode.git"
  :mode "\\.cfg\\'")

(use-package cc-mode
  :custom
  (c-basic-offset tab-width)
  (c-default-style "ellemtel")
  :bind
  (:map c-mode-map
        ("<" . c-electric-lt-gt)
        (">" . c-electric-lt-gt)))

(use-package csharp-mode
  :mode "\\.cs\\'"
  :bind
  (:map csharp-mode-map
        ("<" . c-electric-lt-gt)
        (">" . c-electric-lt-gt)))

(use-package omnisharp
  ;; Use `omnisharp-install-server' to set things up after installing the
  ;; package.
  :config
  (defun m-csharp-mode-setup ()
    "Set up C# mode."
    (omnisharp-mode)
    (add-to-list 'company-backends #'company-omnisharp)
    (add-hook 'before-save-hook #'omnisharp-code-format-entire-file)
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset tab-width)
    (setq truncate-lines t)
    (setq tab-width tab-width)
    (add-to-list 'm-reformatters '(csharp-mode . omnisharp-code-format-entire-file)))
  :hook
  (csharp-mode-hook . m-csharp-mode-setup))

(use-package ahk-mode
  :mode "\\.ahk\\'")

;;;; Utility

(use-package polymode
  :defer 8
  :commands
  pm--get-keylist.keymap-from-parent
  pm--config-name
  :config

  ;; js
  (define-hostmode poly-js-hostmode
    :mode 'js-mode)
  (define-innermode poly-js-graphql-innermode
    :mode 'graphql-mode
    :head-matcher "graphql[ \t\n]*(?`"
    :tail-matcher "`"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-js-mode
    :hostmode 'poly-js-hostmode
    :innermodes '(poly-js-graphql-innermode))

  ;; web
  (define-hostmode poly-web-hostmode
    :mode 'web-mode)
  (define-innermode poly-web-svg-innermode
    :mode 'nxml-mode
    :head-matcher "<svg"
    :tail-matcher "</svg>"
    :head-mode 'inner
    :tail-mode 'inner)
  (define-polymode poly-web-mode
    :hostmode 'poly-web-hostmode
    :innermodes '(poly-web-svg-innermode))

  ;; restclient
  (define-hostmode poly-restclient-hostmode
    :mode 'restclient-mode)
  (define-innermode poly-restclient-elisp-root-innermode
    :mode 'emacs-lisp-mode
    :head-mode 'host
    :tail-mode 'host)
  (define-innermode poly-restclient-elisp-single-innermode
    poly-restclient-elisp-root-innermode
    :head-matcher "^:[^ ]+ :="
    :tail-matcher "\n")
  (define-innermode poly-restclient-elisp-multi-innermode
    poly-restclient-elisp-root-innermode
    :head-matcher "^:[^ ]+ := <<"
    :tail-matcher "^#$")
  (define-polymode poly-restclient-mode
    :hostmode 'poly-restclient-hostmode
    :innermodes '(poly-restclient-elisp-single-innermode
                  poly-restclient-elisp-multi-innermode))

  ;; applescript
  (defun match-string-delimiter (ahead)
    "Match the delimiter of a string, forward if AHEAD is positive.
Backward if AHEAD is negative."
    (let ((re "[^\\]\""))
      (when (or (looking-at re)
                (if (> ahead 0)
                    (re-search-forward re)
                  (re-search-backward re)))
        (cons (match-beginning 0) (match-end 0)))))

  (define-innermode poly-emacs-lisp-apples-innermode
    :mode 'apples-mode
    :head-matcher "do-applescript\s-*.*\""
    :tail-matcher #'match-string-delimiter)
  (define-polymode poly-emacs-lisp-mode
    :hostmode 'poly-emacs-lisp-hostmode
    :innermodes '(poly-emacs-lisp-apples-innermode))

  ;; WIP
  ;; (define-innermode poly-emacs-lisp-Info-innermode
  ;;   :mode 'Info-mode
  ;;   :)

  :hook
  ((js-mode-hook . poly-js-mode)
   ;; (rjsx-mode-hook . poly-rjsx-mode)
   (web-mode-hook . poly-web-mode)
   (restclient-mode-hook . poly-restclient-mode)))

(use-package poly-markdown
  :defer 8
  :hook
  (markdown-mode-hook . poly-markdown-mode))

(use-package fence-edit
  :git "https://github.com/aaronbieber/fence-edit.el.git"
  :config
  (setq fence-edit-blocks
        (append '(("---" "---" yaml)
                  ("+++" "+++" toml)
                  ("graphql[ \t\n]*(?`" "`" graphql)
                  ("<svg" "</svg>" nxml t)
                  ("<html" "</html>" web t)
                  ("<div" "</div>" web t)
                  ;; TODO: How to ignore escaped double quotes? (`\"')
                  ("do-applescript\s-*.*\"" "\"" apples))
                fence-edit-blocks))
  :hook
  ;; Don't shadow the fence-edit binding
  (markdown-mode-hook . (lambda () (bind-key "C-c '" nil markdown-mode-map)))
  :bind
  ("C-c '" . fence-edit-dwim))


(provide 'init)

;;; init.el ends here
