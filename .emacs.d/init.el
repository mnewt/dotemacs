;;; Init.el --- Emacs init file --- -*- lexical-binding: t -*-

;;; Commentary:
;; It's an Emacs init file. Uses `straight.el' for package management and
;; use-package for as much package configuration as possible.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Top Level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are good notes on optimizing startup performance:
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast

;; Set Garbage Collection threshold to 1GB, run GC on idle.
(setq gc-cons-threshold 1073741824)

;; Stolen from
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(run-with-idle-timer 5 nil
                     (lambda ()
                       (nconc file-name-handler-alist file-name-handler-alist-original)
                       (makunbound 'file-name-handler-alist-original)
                       (run-with-idle-timer 20 t (lambda () (garbage-collect)))))

(setq load-prefer-newer t)

(with-eval-after-load 'gnutls
  (setq gnutls-verify-error t))

(with-eval-after-load 'nsm
  (setq network-security-level 'high))

(setq elisp-directory "~/.emacs.d/elisp"
      generated-autoload-file (expand-file-name "loaddefs.el" elisp-directory))

(add-to-list 'load-path elisp-directory)

(load "loaddefs" t)

(defun update-elisp ()
  "Call `update-autoloads-from-directories' on the local lisp directory."
  (interactive)
  (require 'autoload)
  (update-directory-autoloads elisp-directory))
;; (mapc #'byte-compile-file (directory-files elisp-directory t "\\.el$"))
;; (byte-compile-file "~/.emacs.d/init.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cl is assumed to be present in this config and some packages too.
;; (require 'cl)
(require 'seq)

(defun add-multiple-to-list (list items)
  "Run `add-to-list' for all ITEMS in the LIST."
  (seq-do (apply-partially #'add-to-list list) items))

;; These packages are used by many things so they are on the critical path.

(use-package dash
  :commands
  (-map -filter -distinct -interpose))

(use-package s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure the frame
(when window-system
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1)))

(setq frame-resize-pixelwise t
      inhibit-splash-screen t)

(defun display-startup-echo-area-message ()
  "Run when Emacs has finished starting."
  (message "Emacs has finished starting up."))

;; Blinking is NOT OK
(blink-cursor-mode -1)

;; Beeping is REALLY NOT OK
(setq visible-bell t
      ;; Show keystrokes right away, don't show the message in the scratch buffer
      echo-keystrokes 0.1)

;; Smoother and nicer scrolling
(setq scroll-margin 6
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      auto-window-vscroll nil
      mouse-wheel-follow-mouse 't
      mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(pixel-scroll-mode)

(global-hl-line-mode 1)

;; No GUI dialogs
(setq use-dialog-box nil)

;; We don't set a frame title because Emacs on macOS renders the frame title
;; face terribly. No rendering is better than terrible rendering.
(setq frame-title-format nil)
;; No icon in the titlebar
(setq ns-use-proxy-icon nil)

;; Default frame settings. This is actually maximized, not full screen.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; eww uses this as its default font, among others.
(set-face-font 'variable-pitch "Georgia-18")

(use-package doom-themes
  :commands
  (doom-blend))

(use-package solarized-theme)

(defvar theme-source-faces
  '(default mode-line-emphasis mode-line-highlight compilation-mode-line-fail)
  "The faces theme-activate uses to ")

(defvar theme-hook '(doom-themes-visual-bell-config
                     doom-themes-org-config)
  "Run whenever a theme is activated.")

(defvar theme-themes '((doom-vibrant)
                       (doom-one-light)
                       (doom-dracula)
                       (doom-molokai)
                       (doom-tomorrow-day)
                       (solarized-light))
  "Alist where car is the theme and cdr can be:

* A function to run after loading the theme.
* An alist specifying additional arguments. Possible arguments:
** hook - A function, as above.
** specs
** mouse-color")

(defvar theme-current-theme 'doom-dracula
  "Defines the currently loaded theme.")

(defvar theme-specs-common
  '((cursor ((t :background "#F60"))))
  "List of default face specs to apply when a theme is activated.
This list is for specs that are common to all themes and do not
require any kind of dynamic evaluation (e.g. configuring one face
to inherit from another). For dynamic configurations, add an
alist as the cdr of the alist entry in `theme-themes'. The
attributes specified in `theme-themes' overrides these.

For details on face specs see `defface'.")

;; List of current remapping cookies used for face remapping/unmapping.
;; (defvar theme-highlight-window-remapping nil)
;; (make-variable-buffer-local 'theme-highlight-window-remapping)

;; (defface theme-selected-window-face '((t :inherit default))
;;   "The face used for the selected window.")

;; (defun theme-highlight-selected-window ()
;;   "Highlight the selected window with a different background color."
;;   ;; Keep the previous buffer highlighted if the current one is the minibuffer.
;;   (unless (window-minibuffer-p)
;;     (dolist (window (cdr (window-list)))
;;       (with-current-buffer (window-buffer window)
;;         (mapc #'face-remap-remove-relative theme-highlight-window-remapping)
;;         (setq theme-highlight-window-remapping nil))))
;;   (setq theme-highlight-window-remapping
;;         (cl-loop for face in '(default fringe)
;;                  collect (face-remap-add-relative face 'theme-selected-window-face))))

;; (add-hook 'window-configuration-change-hook #'theme-highlight-selected-window)
;; (add-hook 'buffer-list-update-hook #'theme-highlight-selected-window)
;; ;; (remove-hook 'buffer-list-update-hook #'theme-highlight-selected-window)

(defvar theme-selected-window (frame-selected-window)
  "Selected window.")

(defun theme-set-selected-window ()
  "Set the variable `theme-selected-window' appropriately.
This is used to determine whether the current window is active."
  (unless (minibuffer-window-active-p (frame-selected-window))
    (setq theme-selected-window (frame-selected-window))
    (force-mode-line-update)))

;; Executes after a window (not a buffer) has been created, deleted, or moved.
(add-hook 'window-configuration-change-hook #'theme-set-selected-window)

;; Executes after the window manager requests that the user's events
;; be directed to a different frame.
(advice-add 'handle-switch-frame :after #'theme-set-selected-window)

;; Executes after the `buffer-list' changes.
(add-hook 'buffer-list-update-hook #'theme-set-selected-window)

(defun theme-window-active-p ()
  "Return whether the current window is active."
  (eq theme-selected-window (selected-window)))

(defun alist-get-all (key alist &optional default testfn)
  "Return a list of the elements of ALIST with matching KEY.
Modeled on `alist-get', which only returns the first match.

DEFAULT returns a default value if nothing matches.

REMOVE is not implemented on account of I don't care and it's
dumb.

TESTFN is an equality function, *not* an alist function as with
`alist-get'. Default is `eq'."
  (let* ((testfn (or testfn #'eq))
         (matches (seq-filter
                   (lambda (e) (funcall testfn key (car e)))
                   alist)))
    (if matches
        (car (mapcar #'cadr matches))
      default)))

(defun get-attr (object propname attribute name)
  "Get the ATTRIBUTE identified by NAME from PROPNAME."
  (let ((name (if (stringp name) (intern name) name)))
    (cl-some (lambda (e) (when (and (eq attribute (car e)) (eq name (cadr e)))
                           (cadddr e)))
             (get (car object) propname))))

(defun theme-get-attr (attribute name)
  "Get the ATTRIBUTE identified by NAME from the current theme settings.

Example usage

    \(plist-get
      \(face-spec-choose \(theme-get-attr 'theme-face 'default))
      \:background)"
  (get-attr custom-enabled-themes 'theme-settings attribute name))

(defun theme-get-face (face)
  "Get the FACE from the current theme. See `theme-get-attr'."
  (theme-get-attr 'theme-face face))

(defun theme-get-value (value)
  "Get the VALUE from the current theme. See `theme-get-attr'."
  (theme-get-attr 'theme-value value))

(defun theme-search-attrs (regexp)
  "Return the attributes in the current theme which match the REGEXP."
  (seq-filter (lambda (e) (string-match-p regexp (symbol-name (cadr e))))
              (get (car custom-enabled-themes) 'theme-settings)))

(defun theme-generate-specs ()
  "Automatically generate theme specs the supplied faces.

See also `theme-specs-common'. Advise or override this function
to customize furter."
  (let* ((default-spec (face-spec-choose (theme-get-face 'default)))
         (outline-1-spec (face-spec-choose (theme-get-face 'outline-1)))
         (active-bg (plist-get default-spec :background))
         (active-fg (plist-get default-spec :foreground))
         (inactive-bg (doom-blend active-bg active-fg 0.95))
         (inactive-fg (doom-blend active-bg active-fg 0.4))
         (highlight-fg (plist-get outline-1-spec :foreground)))
    `((default ((t :background ,inactive-bg)))
      ;; (theme-selected-window-face ((t :background ,active-bg)))
      (window-highlight-focused-window ((t :background ,active-bg)))
      (fringe ((t :background ,inactive-bg)))
      (vertical-border ((t :foreground ,inactive-bg)))
      (mode-line ((t :box nil :underline nil
                     :background ,inactive-bg
                     :foreground ,(doom-blend active-fg active-bg 0.9))))
      (mode-line-emphasis ((t :background ,(doom-blend active-bg active-fg 0.7)
                              :foreground ,active-fg)))
      (mode-line-highlight ((t :background ,highlight-fg
                               :foreground ,active-bg)))
      (mode-line-buffer-id ((t :background ,(doom-blend active-bg active-fg 0.2)
                               :foreground ,inactive-bg :bold t)))
      (compilation-mode-line-fail ((t :inherit highlight)))
      (doom-modeline-error ((t :background nil :foreground nil :inherit highlight)))
      (mode-line-inactive ((t :box nil :underline nil
                              :background ,inactive-bg
                              :foreground ,inactive-fg)))
      (eyebrowse-mode-line-active ((t :foreground ,highlight-fg)))
      (sp-show-pair-match-face ((t :foreground nil :background nil
                                   :inherit highlight))))))

(defun theme-activate (theme)
  "Switch the current Emacs theme to THEME.

Handle some housekeeping that comes with switching themes. Set
face specs for the mode-line. Having done that try to prevent
Emacs from barfing fruit salad on the screen."
  (custom-set-variables '(custom-enabled-themes nil))
  (load-theme (if (stringp theme) (intern theme) theme) t)
  (let* ((opts (alist-get theme theme-themes)))
    (setq opts (append opts `((specs ,(theme-generate-specs)))))
    ;; Feed face specs to `custom-set-faces' in reverse because last write wins.
    ;; We do it this way so additional specs can be specified when adding the
    ;; theme to `theme-themes'.
    (apply #'custom-set-faces
           (append theme-specs-common (reverse (alist-get-all 'specs opts))))
    (let-alist opts
      (set-mouse-color (cond
                        ((boundp 'mouse-color) mouse-color)
                        ((equal 'dark (frame-parameter nil 'background-mode)) "white")
                        (t "black")))
      (when (boundp 'hook) (mapc #'funcall hook)))
    (setq theme-current-theme theme)
    (mapc #'funcall theme-hook)))

(defun theme-choose (theme)
  "Interactively choose a THEME from `theme-themes' and activate it."
  (interactive (list (completing-read "Load custom theme: "
                                      (mapcar #'car theme-themes)
                                      nil t nil
                                      'theme-choose-history)))
  (theme-activate (intern theme)))

(defun theme-render-mode-line (left right)
  "Return a string string concatenating LEFT and RIGHT.

Insert spaces between the two so that the string is
`window-total-width' columns wide."
  (let ((left (apply #'concat left))
        (right (apply #'concat right)))
    ;; Start with a string so left can start with nil without breaking things.
    (concat ""
            left
            ;; ?\s is a space character
            (make-string (- (window-total-width) (length left) (length right)) ?\s)
            right)))

(defun theme-ml-concat (strings &optional separator outside)
  "Concatenate list of STRINGS, optionally with SEPARATOR in the
  middle."
  (let* ((separator (or separator " "))
         (outside (when outside separator))
         (inside (string-join (cl-remove-if-not (lambda (s) (and s (> (length s) 0)))
                                                strings)
                              separator)))
    (when (> (length inside) 0)
      (concat outside inside outside))))

(defun theme-ml-remote-hostname ()
  "Return the remote hostname for the current buffer or `nil' if
  local."
  (when (file-remote-p default-directory)
    (concat " "
            (tramp-file-name-host (tramp-dissect-file-name default-directory))
            " ")))

(defun theme-ml-term-mode ()
  "Return the input mode for the buffer if in `term-mode' or
`nil' otherwise."
  (when (eq major-mode 'term-mode)
    (cond
     ((term-in-char-mode) " [char] ")
     ((term-in-line-mode) " [line] ")
     (t ""))))

(defun theme-ml-evil ()
  "Return the state of `evil-mode', or `nil' if it's not
  enabled."
  (when (bound-and-true-p evil-state)
    (cl-case evil-state
      (normal (propertize " NORMAL " 'face
                          `(:foreground
                            "black"
                            :background
                            ,(aref (theme-get-value 'ansi-color-names-vector) 2))))
      (insert (propertize " INSERT " 'face
                          `(:foreground
                            "white"
                            :background
                            ,(aref (theme-get-value 'ansi-color-names-vector) 4))))
      (t (propertize      "  EVIL  " 'face
                          `(:foreground
                            ,(aref (theme-get-value 'ansi-color-names-vector) 0)
                            :background
                            ,(aref (theme-get-value 'ansi-color-names-vector) 7)))))))

(defun when-propertize (exp &rest properties)
  "Propertize the result of body or return `nil'."
  (when exp (apply #'propertize exp properties)))

(setq-default
 mode-line-format
 '((:eval
    (if (theme-window-active-p)
        (theme-render-mode-line
         ;; left
         (list
          (when-propertize (theme-ml-remote-hostname) 'face 'mode-line-highlight)
          (propertize (concat " " (buffer-name) " ") 'face 'mode-line-buffer-id)
          (when (buffer-modified-p) " • ")
          (theme-ml-evil))
         ;; right
         (list
          (when-propertize (theme-ml-term-mode) 'face 'mode-line-emphasis)
          ;; Some modes, e.g. `dired+', set `mode-name' to something fancy that
          ;; must be evaluated with `format-mode-line'.
          (concat " "(format-mode-line mode-name) " ")
          (when (fboundp #'eyebrowse-mode-line-indicator)
            (concat (eyebrowse-mode-line-indicator) " "))
          (when-propertize
           (theme-ml-concat
            (list (when (buffer-narrowed-p) "⒩")
                  (when (bound-and-true-p hs-minor-mode) "⒣")
                  (when (bound-and-true-p outline-minor-mode) "⦿"))
            " "
            t)
           'face 'mode-line-emphasis)
          (propertize (s-pad-left 8 " " (format-mode-line " %l:%c  "))
                      'face 'mode-line-buffer-id)))
      (theme-render-mode-line
       ;; left
       (list
        " "
        (buffer-name)
        " "
        (when (buffer-modified-p) " • "))
       ;; right
       (list ""))))))

(add-hook 'after-init-hook (lambda () (theme-activate theme-current-theme)))

(use-package window-highlight
  :if (>= emacs-major-version 27)
  :straight
  (:type git :host github :repo "dcolascione/emacs-window-highlight")
  :hook
  (after-init . window-highlight-mode))

(bind-key "C-c C-t" #'theme-choose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment and Operating System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Path
(defvar set-path-unix nil
  "Defines a list of path entries to add to *NIX systems.")

(defvar set-path-windows
  '("C:/bin"
    "C:/Program Files/Emacs/bin")
  "Defines a list of path entries to add to Windows systems.")

(defvar set-path-user nil
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

(defun set-path ()
  "Set path variables correctly for Linux, macOS, or Windows."
  (let* ((os-specific-paths (if (eq system-type 'windows-nt)
                                set-path-windows
                              set-path-unix))
         (sep (if (eq system-type 'windows-nt) ";" ":"))
         (old-path (split-string (getenv "PATH") sep))
         ;; De-dupe and validate new path
         (new-path
          (-map 'expand-file-name
                (-filter 'file-directory-p
                         (-distinct (append set-path-user
                                            os-specific-paths
                                            old-path))))))
    (setenv "PATH" (apply 'concat (-interpose sep new-path)))
    ;; (message "New path: %s" new-path)
    (setq exec-path new-path)))

(source-sh "~/.env")
(source-sh "~/.bin/start-ssh-agent")
(set-path)

(add-hook 'after-init-hook (lambda ())
          (require 'server)
          (unless (server-running-p) (server-start)))

(use-package restart-emacs
  :commands
  (restart-emacs))

(defun config-unix ()
  "Configure Emacs for common Unix (Linux and macOS) settings."
  nil)

(defun config-linux ()
  "Configure Emacs for Linux."
  (config-unix)
  (set-face-font 'default "DejaVu Sans-12"))

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
  (when window-system (menu-bar-mode +1))
  (set-face-font 'default "Monaco-13")
  ;; Use system trash
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash"))

(declare w32-pass-lwindow-to-system)
(declare w32-lwindow-modifier)
(declare w32-pass-rwindow-to-system)
(declare w32-rwindow-modifier)

(defun config-windows ()
  "Configure Emacs for Windows."
  (menu-bar-mode -1)
  (setq w32-pass-lwindow-to-system nil
        w32-lwindow-modifier 'super
        w32-pass-rwindow-to-system nil
        w32-rwindow-modifier 'super
        os-open-file-executable "explorer")
  (set-face-font 'default "Lucida Console-12"))

;; OS specific configuration
(pcase system-type
  ('darwin (config-macos))
  ('gnu/linux (config-linux))
  ('windows-nt (config-windows))
  ('cygwin (config-windows)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; savehist
(savehist-mode 1)
(setq savehist-autosave-interval 60
      history-length 200
      history-delete-duplicates t
      savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring
                                      file-name-history
                                      magit-read-rev-history
                                      read-expression-history
                                      command-history
                                      extended-command-history
                                      ivy-history))

;; Restore point location in file when opening it.
(save-place-mode 1)

;; Recent files.
(recentf-mode 1)
(setq recentf-max-saved-items 100
      recentf-max-menu-items 15
      ;; Disable recentf-cleanup on Emacs start because it can cause problems
      ;; with remote files. Clean up on idle for 60 seconds.
      recentf-auto-cleanup 60)

;; Track directories in the recentf list
(defun recentd-track-opened-file ()
  "Insert the name of the directory just opened into the recent list."
  (and (derived-mode-p 'dired-mode) default-directory
       (recentf-add-file default-directory))
  ;; Must return nil because it is run from `write-file-functions'.
  nil)
(add-hook 'dired-after-readin-hook #'recentd-track-opened-file)

;; Store all backup and autosave files in their own directory since it is bad to
;; clutter project directories.
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
      ;; Don't create `#filename' lockfiles in $PWD. Lockfiles are useful but it
      ;; generates too much activity from tools watching for changes during
      ;; development.
      create-lockfiles nil
      ;; Increase undo limit to 3MB per buffer.
      undo-limit 3145728)

;; Whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on disk.
(global-auto-revert-mode 1)
;; Auto refresh dired
(setq global-auto-revert-non-file-buffers t
      ;; Don't print auto revert messages.
      auto-revert-verbose nil)

;; Desktop
(require 'desktop)
(setq desktop-dirname "~/.emacs.d")
(add-to-list 'desktop-globals-to-save 'kill-ring)
(add-to-list 'desktop-globals-to-save 'theme-current-theme)
(desktop-save-mode 1)

(defun psync-maybe-sync ()
  "If we find a `psync_config' file then run `psync'."
  (interactive)
  (let ((default-directory (or (and (fboundp 'projectile-project-root)
                                    (projectile-project-root))
                               default-directory)))
    (when (and (executable-find "psync")
               (file-exists-p (expand-file-name "psync_config")))
      (unless (= 0 (shell-command-exit-code "psync"))
        (message "psync in directory %s failed." default-directory)))))

(add-hook 'after-save-hook #'psync-maybe-sync)

(bind-key "C-x M-s" #'psync-maybe-sync)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Full screen
(defun fullscreen ()
  "Toggle fullscreen mode."
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(bind-keys ("C-s-f" . fullscreen))

;; Change yes/no prompts to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable all commands
(setq disabled-command-function nil)

(defun undo-tree-keep-region (f &rest args)
  "Keep region after `undo-tree-undo'."
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        (apply f args)
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    (call-interactively f)))

(use-package undo-tree
  :straight
  (:type git :host nil :repo "http://www.dr-qubit.org/git/undo-tree.git")
  :init
  ;; Keep region when undoing in region.
  ;; http://whattheemacsd.com/my-misc.el-02.html
  (advice-add 'undo-tree-undo :around #'undo-tree-keep-region)
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  :hook
  (after-init . global-undo-tree-mode)
  :bind
  (("s-z" . undo-tree-undo)
   ("s-Z" . undo-tree-redo)
   ("s-y" . undo-tree-redo)
   ("M-s-z" . undo-tree-visualize)))

(defun evil-mode-toggle ()
  "Toggle `evil-mode'.
It's necessary because it often forgets to change the cursor type back."
  (interactive)
  (if (bound-and-true-p evil-state)
      (progn
        (call-interactively #'turn-off-evil-mode)
        (setq cursor-type 'box))
    (call-interactively #'turn-on-evil-mode)))

(use-package evil
  :commands
  (turn-on-evil-mode turn-off-evil-mode)
  :bind
  ("s-ESC" . evil-mode-toggle)
  ("s-<escape>" . evil-mode-toggle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show line in the original buffer from occur mode
(setq list-matching-lines-jump-to-current-line t)

(use-package winner
  :hook
  (after-init . winner-mode)
  :bind
  (("C-c [" . winner-undo)
   ("s-[" . winner-undo)
   ("C-c ]" . winner-redo)
   ("s-]" . winner-redo)))

(use-package buffer-move
  :bind
  (("C-H-W" . buf-move-up)
   ("C-H-S" . buf-move-down)
   ("C-H-A" . buf-move-left)
   ("C-H-D" . buf-move-right)))

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  ("s-w" . ace-delete-window)
  ("s-W" . ace-delete-other-windows))

(use-package winum
  :custom
  (winum-auto-setup-mode-line nil)
  :hook
  (after-init . winum-mode)
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
  :custom
  (eyebrowse-new-workspace t)
  (eyebrowse-mode-line-separator " ")
  :hook
  (after-init . eyebrowse-mode)
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
      initial-major-mode 'lisp-interaction-mode)

;; Try to re-use help buffers of different sorts
(setq display-buffer-alist
      `((,(rx bos
              (or "*Apropos*" "*eww*" "*Help*" "*helpful" "*info*" "*Summary*")
              (0+ not-newline))
         (display-buffer-reuse-mode-window display-buffer-pop-up-window)
         (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))

;; Make `emacsclient' support solon notation of line:column
(advice-add 'server-visit-files :around #'wrap-colon-notation)

;; Just set up 3 windows, no fancy frames or whatever
(with-eval-after-load 'ediff
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package goto-addr
  :hook
  ((compilation-mode text-mode eshell-mode shell-mode) . goto-address-mode)
  (prog-mode . goto-address-prog-mode)
  :bind
  (:map goto-address-highlight-keymap
        ("C-c C-o" . goto-address-at-point)))

;; outline-mode extension for navigating by sections. in Emacs Lisp that is defined by
;; `;;; ', `;;;; ', etc. Everywhere else it is like `;; * ' `;; ** ', and so on.
(use-package outshine
  :init
  (defvar outline-minor-mode-prefix "\M-#")
  :config
  (put 'narrow-to-region 'disabled nil)
  ;; Narrowing now works within the headline rather than requiring to be on it
  (advice-add 'outshine-narrow-to-subtree :before
              (lambda (&rest _args) (unless (outline-on-heading-p t)
                                      (outline-previous-visible-heading 1))))
  :hook
  (prog-mode . outshine-mode)
  :bind
  (:map outline-minor-mode-map
        ;; Don't shadow smarparens or org bindings
        ("M-<up>" . nil)
        ("M-<down>" . nil)
        ("M-=" . outline-show-current-sublevel)
        ("M-p" . outline-subtree-previous)
        ("M-n" . outline-subtree-next)))

;; hs-minor-mode for folding top level forms
(use-package hideshow
  :custom
  (hs-hide-comments-when-hiding-all nil)
  :bind
  ("C-<tab>" . hs-toggle-hiding)
  :hook
  (hs-minor-mode . hs-hide-all))

(use-package symbol-overlay
  :hook
  (prog-mode . symbol-overlay-mode)
  :bind
  ("C-s-n" . symbol-overlay-jump-next)
  ("C-s-p" . symbol-overlay-jump-prev)
  ("C-s-r" . symbol-overlay-rename)
  ("C-s-5" . symbol-overlay-query-replace))

(use-package rainbow-mode
  :hook
  ((css-mode emacs-lisp-mode sass-mode) . rainbow-mode))

(use-package crux
  :bind
  ("C-c C-j" . crux-eval-and-replace)
  ("C-s-<backspace>" . crux-kill-line-backwards)
  ("C-M-X" . crux-indent-defun)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("C-c R" . crux-rename-file-and-buffer)
  ("C-c k" . crux-kill-other-buffers)
  ("C-c O" . crux-open-with)
  ("C-c S" . crux-find-shell-init-file))

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
 ("s-x" . kill-line-or-region)
 ("s-c" . copy-line-or-region)
 ("s-v" . clipboard-yank-and-indent)
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
 ("s-/" . comment-toggle)
 ("s-h" . ns-do-hide-emacs)
 ("s-H" . ns-do-hide-others)

 ("C-c U" . revert-buffer)
 ("C-c i" . os-reveal-file)
 ("s-<return>" . eval-last-sexp)
 ("s-RET" . eval-last-sexp)
 ("s-n" . new-scratch-buffer)
 ("s-N" . new-scratch-buffer-other-window)
 ("C-c C-n" . new-scratch-buffer)
 ("C-c M-n" . new-scratch-buffer-other-window)
 ("C-S-p" . previous-line-4)
 ("C-S-n" . next-line-4)
 ("H-p" . "\C-u1\M-v")
 ("H-n" . "\C-u1\C-v")

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
 ("H-h" . windmove-left)
 ("H-d" . windmove-right)
 ("H-l" . windmove-right)
 ("H-w" . windmove-up)
 ("H-j" . windmove-up)
 ("H-s" . windmove-down)
 ("H-k" . windmove-down)
 ("M-]" . windmove-right)
 ("M-[" . windmove-left)

 ;; Resize windows
 ("M-s-<up>" . shrink-window)
 ("M-s-<down>" . enlarge-window)
 ("M-s-<left>" . shrink-window-horizontally)
 ("M-s-<right>" . enlarge-window-horizontally)

 ;; Navigate with mark
 ("M-s-," . pop-to-mark-command)
 ("C-c ," . pop-to-mark-command)
 ("s-," . pop-global-mark)
 ("C-c C-," . pop-global-mark)

 ;; Kill buffer and window at the same time.
 ("M-s-w" . kill-buffer-and-window)
 ("M-s-W" . kill-other-buffer-and-window)

 ;; Tags
 ("s-R" . xref-find-definitions-other-window)
 ("C-c M-r" . xref-find-definitions-other-window)

 ("C-c C-f" . find-file-at-point-with-line)

 ;; Init
 ("C-c I" . (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

 :map ctl-x-4-map
 ("t" . toggle-window-split))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help and Documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq suggest-key-bindings 5
      ;; Select help window so it's easy to quit it with `q'
      help-window-select t)

(use-package help-at-pt
  :custom
  (help-at-pt-display-when-idle t)
  :hook
  (after-init . help-at-pt-set-timer))

(use-package help-fns+)

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

(bind-keys
 ("C-h C-i" . #'elisp-index-search)
 ("C-h M-i" . #'info-apropos))

(global-eldoc-mode)

;; ELDoc
(seq-do (lambda (m) (add-hook m #'turn-on-eldoc-mode))
        '(emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          ielm-mode-hook))

(use-package which-key
  :hook
  (after-init . which-key-mode)
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

(use-package tldr
  :init
  (unbind-key "C-h t")
  :custom
  (tldr-enabled-categories '("common" "linux" "osx"))
  :bind
  ("C-h t t" . tldr)
  ("C-h t u" . tldr-update-docs))

(use-package eg
  ;; :ensure-system-package
  ;; (eg . "pip install eg")
  :straight
  (:type git :host github :repo "mnewt/eg.el")
  :bind
  ("C-h e" . eg))

(defun dash-docs-installed-docsets ()
  "Return a list of the currently installed docsets."
  (mapcar (lambda (f) (string-trim-right f ".docset"))
          (directory-files dash-docs-docsets-path nil "[^.]*\.docset")))

(use-package dash-docs
  :straight
  (:type git :host github :repo "gilbertw1/dash-docs")
  :custom
  (dash-docs-docsets-path "~/.config/docsets")
  (dash-docs-browser-func #'eww)
  (dash-docs-common-docsets (dash-docs-installed-docsets)))

(use-package counsel-dash
  ;; :ensure-system-package sqlite3
  :straight
  (:type git :host github :repo "gilbertw1/counsel-dash")
  :commands
  (counsel-dash counsel-dash-at-point counsel-dash-install-docset)
  :bind
  ("M-s-l" . counsel-dash)
  ("C-h C-d" . counsel-dash)
  ("M-s-." . counsel-dash-at-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Delete selection on insert or yank
(delete-selection-mode 1)

;; Tabs
(setq-default indent-tabs-mode nil
              tab-width 2
              tab-stop-list (number-sequence tab-width 120 tab-width))

(defun configure-auto-fill-mode ()
  "Automatically fill comments.
Wraps on `fill-column' columns."
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'prog-mode-hook #'configure-auto-fill-mode)

;; sh-mode
(use-package sh-script
  ;; :ensure-system-package shfmt
  :custom
  (sh-basic-offset tab-width)
  (sh-indentation tab-width)
  ;; Tell `executable-set-magic' to insert #!/usr/bin/env interpreter
  (executable-prefix-env t)
  :hook
  ;; Make a shell script executable automatically on save
  (after-save . executable-make-buffer-file-executable-if-script-p)
  (before-save . maybe-reset-major-mode)
  :bind
  (:map sh-mode-map
        ("s-<ret>" . eshell-send-current-line)))

;; dw (https://gitlab.com/mnewt/dw)
(add-to-list 'auto-mode-alist '("\\DWfile.*\\'" . sh-mode))

;; Automatically indent after RET
(electric-indent-mode +1)

(use-package volatile-highlights
  :hook
  (after-init . (lambda ()
                  (vhl/define-extension 'undo 'redo 'undo-modern)
                  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                                        'evil-paste-pop 'evil-move)
                  (vhl/install-extension 'evil)
                  (volatile-highlights-mode t)))
  :commands
  (volatile-highlights-mode vhl/define-extension vhl/install-extension
                            vhl/disable-advice-if-defined))

(use-package goto-chg
  :bind
  ("C-." . goto-last-change)
  ("C-;" . goto-last-change-reverse))

(use-package easy-kill
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark)))

(use-package ace-jump-zap
  :bind
  ("M-z" . ace-jump-zap-up-to-char-dwim)
  ("C-M-z" . ace-jump-zap-to-char-dwim))

(use-package mwim
  :bind
  ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
  ([remap move-end-of-line] . mwim-end-of-code-or-line))

(use-package expand-region
  :custom
  (expand-region-fast-keys-enabled nil)
  :bind
  ("s-d" . er/expand-region)
  ("C-=" . er/expand-region)
  ("s-D" . er/contract-region)
  ("C-+" . er/contract-region))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("M-s-m" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/unmark-next-like-this)
  ("C-M-<" . mc/mark-previous-like-this)
  ("C-M->" . mc/unmark-previous-like-this)
  ("C-c >" . mc/mark-all-dwim)
  ("C-c C-a"  . mc/mark-all-dwim))

(use-package move-text
  :bind
  (:map prog-mode-map
        ("M-S-<up>" . move-text-up)
        ("M-S-<down>" . move-text-down)))

(use-package string-inflection
  :bind
  ("C-c C-u" . string-inflection-all-cycle))

(use-package yasnippet
  :custom
  (yas-verbosity 1)
  :hook
  (after-init . yas-global-mode)
  :bind
  ("C-c C-s" . yas-insert-snippet))

(use-package yasnippet-snippets
  :defer 2)

(use-package flycheck
  :custom
  (flycheck-check-syntax-automatically '(save mode-enable))
  :hook
  (js2-mode . flycheck-mode)
  :bind
  ("C-c ! !" . flycheck-mode))

(use-package format-all
  :hook
  ((css-mode
    dockerfile-mode
    emacs-lisp-mode
    enh-ruby-mode
    go-mode
    lua-mode
    php-mode
    ruby-mode
    sh-mode
    toml-mode
    web-mode
    yaml-mode) . format-all-mode))

(use-package darkroom
  :bind
  ("C-c C-d" . darkroom-mode)
  :commands
  (darkroom-mode))

(bind-keys ("C-M-\\" . indent-buffer-or-region)
           ("C-\\" . indent-defun)
           ("C-^" . delete-indentation-forward)
           ("s-C" . copy-line-or-region-to-other-window)
           ("s-X" . move-line-or-region-to-other-window)
           ;; Replace `delete-horizontal-space' with the more useful `cycle-spacing'.
           ("M-\\" . cycle-spacing)
           ;; Continue comment on next line (default binding is "C-M-j")
           ("M-RET" . indent-new-comment-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lisp, S-Expressions, Parentheses, Brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("Cask\\'" emacs-lisp-mode))

(use-package parinfer
  :custom
  (parinfer-extensions
   '(defaults       ; should be included.
      pretty-parens ; different paren styles for different modes.
      smart-tab     ; C-b & C-f jump positions and smart shift with tab & S-tab.
      smart-yank))  ; Yank behavior depends on mode.
  :hook
  ((clojure-mode common-lisp-mode emacs-lisp-mode hy-mode lisp-interaction-mode
                 lisp-mode scheme-mode) . parinfer-mode)
  (parinfer-mode . (lambda () (parinfer-strategy-add 'default 'newline-and-indent)))
  :commands
  (parinfer-strategy-add)
  :bind
  (:map parinfer-mode-map
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)
        ("C-i" . parinfer--reindent-sexp)
        ("C-M-i" . parinfer-auto-fix)
        ("C-," . parinfer-toggle-mode)
        ;; Don't interfere with smartparens quote handling
        ("\"" . nil)
        ;; sp-newline seems to offer a better experience for lisps
        ("RET" . sp-newline)
        ("<return>" . sp-newline)
        :map parinfer-region-mode-map
        ("C-i" . indent-for-tab-command)
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)))

(use-package smartparens
  :custom
  (sp-hybrid-kill-entire-symbol nil)
  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (sp-cancel-autoskip-on-backward-movement nil)
  ;; smartparens does some weird stuff with bindings so you can't reliably use
  ;; `use-package/:bind' to set them.
  (sp-base-key-bindings 'paredit)
  (sp-override-key-bindings '(("C-M-<backspace>" . sp-splice-sexp-killing-backward)))
  :hook
  (smartparens-mode . (lambda ()
                        (require 'smartparens-config) (turn-on-show-smartparens-mode)))
  (prog-mode . turn-on-smartparens-mode)
  (clojure-mode . (lambda () (require 'smartparens-clojure)))
  ((ruby-mode enh-ruby-mode) . (lambda () (require 'smartparens-ruby)))
  ((javascript-mode js2-mode json-mode) . (lambda () (require 'smartparens-javascript)))
  (lua-mode . (lambda () (require 'smartparens-lua)))
  (markdown-mode . (lambda () (require 'smartparens-markdown)))
  (org-mode . (lambda () (require 'smartparens-org)))
  (python-mode . (lambda () (require 'smartparens-python)))
  (text-mode . (lambda () (require 'smartparens-text)))
  (web-mode . (lambda () (require 'smartparens-html)))
  (after-init
   .
   (lambda ()
     (sp-with-modes '(c-mode c++-mode css-mode graphql-mode javascript-mode
                             js2-mode json-mode objc-mode python-mode java-mode
                             sh-mode web-mode)
       (sp-local-pair "{" nil :post-handlers '((sp-create-newline-and-enter-sexp "RET")))
       (sp-local-pair "[" nil :post-handlers '((sp-create-newline-and-enter-sexp "RET")))
       (sp-local-pair "(" nil :post-handlers '((sp-create-newline-and-enter-sexp "RET"))))
     (sp-with-modes
         '(python-mode)
       (sp-local-pair "\"\"\"" "\"\"\""
                      :post-handlers '((sp-create-newline-and-enter-sexp "RET"))))
     (sp-with-modes
         '(sh-mode)
       (sp-local-pair "do" "done"
                      :when '(("SPC" "RET"))
                      :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
                      :actions '(insert navigate)
                      :pre-handlers '(sp-sh-pre-handler)
                      :post-handlers '(sp-sh-block-post-handler))
       (sp-local-pair "then" "fi"
                      :when '(("SPC" "RET"))
                      :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
                      :actions '(insert navigate)
                      :pre-handlers '(sp-sh-pre-handler)
                      :post-handlers '(sp-sh-block-post-handler))
       (sp-local-pair "case" "esac"
                      :when '(("SPC" "RET"))
                      :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
                      :actions '(insert navigate)
                      :pre-handlers '(sp-sh-pre-handler)
                      :post-handlers '(sp-sh-block-post-handler))
       (smartparens-global-mode))))
  :commands
  (sp-local-pair sp-with-modes smartparens-global-mode turn-on-show-smartparens-mode)
  :bind
  (:map smartparens-mode-map
        ("C-c C-<return>" . toggle-sp-newline)
        :filter (cl-some #'derived-mode-p sp-lisp-modes)
        ([remap kill-line] . sp-kill-hybrid-sexp)))

(use-package clojure-mode
  :config
  (add-to-list 'interpreter-mode-alist '("inlein" . clojure-mode))
  :hook
  ((clojure-mode clojurescript-mode) . turn-on-eldoc-mode))

;; (use-package clojure-mode-extra-font-locking
;;   :defer 1)

(use-package cider
  ;; :hook
  ;; (cider-repl-mode . (lambda () (company-mode nil)))
  :commands
  (cider-switch-to-repl-buffer)
  :bind
  (:map cider-mode-map
        ("s-<return>" . cider-eval-last-sexp)))

(use-package inf-clojure
  :hook
  (comint-mode . reinstate-comint-simple-send)
  :bind
  (:map inf-clojure-minor-mode-map
        ("s-<return>" . inf-clojure-eval-last-sexp)
        ("C-c C-k" . inf-clojure-eval-buffer)))

;; (use-package sly
;;   ;; There are some problems building sly with straight.el in Windows
;;   :unless (eq system-type 'windows-nt)
;;   :custom
;;   (inferior-lisp-program (executable-find "sbcl"))
;;   :bind
;;   (:map sly-prefix-map
;;         ("M-h" . sly-documentation-lookup)))

;; (use-package sly-company
;;   :unless (eq system-type 'windows-nt)
;;   :hook
;;   (sly-mode . sly-company-mode)
;;   :config
;;   (add-to-list 'company-backends 'sly-company))

;; Configured to use CHICKEN Scheme
;; (use-package geiser
;;   :custom
;;   (geiser-default-implementation 'chicken)
;;   (geiser-mode-eval-last-sexp-to-buffer t)
;;   (scheme-program-name "csi -:c")
;;   :config
;;   (setq-default geiser-scheme-implementation 'chicken)

;;   ;; Indenting module body code at column 0
;;   (defun scheme-module-indent (state indent-point normal-indent) 0)
;;   (put 'module 'scheme-indent-function 'scheme-module-indent)
;;   (put 'and-let* 'scheme-indent-function 1)
;;   (put 'parameterize 'scheme-indent-function 1)
;;   (put 'handle-exceptions 'scheme-indent-function 1)
;;   (put 'when 'scheme-indent-function 1)
;;   (put 'unless 'scheme-indenfunction 1)
;;   (put 'match 'scheme-indent-function 1)
;;   :commands
;;   (geiser run-geiser run-chicken))

(bind-keys
 :map lisp-mode-shared-map
 ("s-<return>" . eval-last-sexp)
 ("C-s-<return>" . eval-last-sexp-other-window)
 ("C-c C-k" . eval-buffer)
 ("C-x C-r" . eval-region)
 ("C-x r E" . expression-to-register)
 ("C-x r e" . eval-register))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shell, Terminal, SSH, Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'dired (require 'm-dired))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(with-eval-after-load 'tramp (require 'm-shell-common))

(with-eval-after-load 'shell
  (require 'm-shell-common)
  (bind-keys :map shell-mode-map
             ("C-d" . comint-delchar-or-eof-or-kill-buffer)
             ("SPC" . comint-magic-space)))

(with-eval-after-load 'term (require 'm-shell-common))

;; TODO: Is there some way to check whether module support is compiled in?
(ignore-errors
  (let ((vterm-dir "~/code/emacs-libvterm"))
    (when (file-exists-p vterm-dir)
      (add-to-list 'load-path "~/code/emacs-libvterm")
      (let (vterm-install)
        (require 'vterm)))))

(use-package term
  :bind
  (("C-c t" . vterm)
   :map term-mode-map
   ("M-p" . term-send-up)
   ("M-n" . term-send-down)
   :map term-raw-map
   ("M-o" . other-window)
   ("M-p" . term-send-up)
   ("M-n" . term-send-down)
   ("C-M-j" . term-switch-to-shell-mode)))

;; xterm colors
(use-package xterm-color
  :custom
  (comint-output-filter-functions
   (remove 'ansi-color-process-output comint-output-filter-functions))
  :config
  (advice-add 'shell-command :after #'xterm-color-apply-on-minibuffer-advice)
  :commands
  (xterm-color-filter)
  :hook
  (shell-mode
   . (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
  (compilation-start
   . (lambda (proc)
       ;; We need to differentiate between compilation-mode buffers
       ;; and running as part of comint (which at this point we assume
       ;; has been configured separately for xterm-color)
       (when (eq (process-filter proc) 'compilation-filter)
         ;; This is a process associated with a compilation-mode buffer.
         ;; We may call `xterm-color-filter' before its own filter function.
         (set-process-filter proc
                             (lambda (proc string)
                               (funcall 'compilation-filter proc
                                        (xterm-color-filter string))))))))

(use-package bash-completion
  :custom
  ;; So that it doesn't sometimes insert a space ('\ ') after completing the
  ;; file name.
  (bash-completion-nospace t)
  :hook
  (shell-dynamic-complete-functions . bash-completion-dynamic-complete))

(use-package fish-mode
  :custom (fish-indent-offset tab-width)
  :mode "\\.fish\\'")

(use-package fish-completion
  :custom
  (fish-completion-fallback-on-bash-p t)
  :hook
  (after-init . global-fish-completion-mode))

(use-package company-shell
  :config
  (add-to-list
   'company-backends
   `(company-shell company-shell-env
                   ,(when (executable-find "fish") 'company-fish-shell))))

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
  :config
  (advice-add 'eshell-ls-decorated-name :around #'m-eshell-ls-decorated-name)
  :hook
  ((eshell-before-prompt . eshell/init)
   (eshell-before-prompt . (lambda ()
                             (setq xterm-color-preserve-properties t)
                             (rename-buffer
                              (format "*%s*" default-directory) t))))
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

(use-package pinentry
  ;; TODO: Don't know how to get pinentry to work with Windows. Maybe a TCP socket?
  :unless (eq system-type 'windows-nt)
  :custom
  (password-cache-expiry nil)
  :config
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  :hook
  (after-init . pinentry-start))

(bind-keys ("C-c C-v" . expand-environment-variable)
           ("C-:" . tramp-insert-remote-part))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes, Journal, and Documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'org
  (require 'm-org)
  (bind-keys  :map (org-mode-map ("s-;" . org-shiftright))))

;; visual-line-mode
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :hook
  (after-init . pdf-loader-install)
  (pdf-view-mode . (lambda () (auto-revert-mode -1)))
  :bind
  (:map pdf-view-mode-map
        ("s-f" . isearch-forward)))

(bind-keys
 ("C-c l" . org-store-link)
 ("C-c a" . org-agenda)
 ("C-c c" . org-capture)
 ("C-c b" . org-switchb)
 ("C-c s" . search-org-files)
 ("C-c n" . (lambda () (interactive) (find-file (expand-file-name "new-note.org"))))
 ("C-c o" . (lambda () (interactive) (find-file org-directory)))
 :map visual-line-mode-map
 ;; Don't shadow mwim and org-mode bindings
 ([remap move-beginning-of-line] . nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Log files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vlf
  :custom
  (vlf-application 'dont-ask)
  :config
  (require 'vlf-setup))

;; (use-package logview)

(with-eval-after-load 'dired
  (bind-key "F" #'tail-file dired-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search, Completion, Symbols, Project Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show line in the original buffer from occur mode
(setq list-matching-lines-jump-to-current-line t)

(use-package re-builder
  :custom
  ;; string syntax means you don't need to double escape things.
  (reb-re-syntax 'string)
  :bind
  ("C-c r" . re-builder))

(use-package pcre2el
  :hook
  ((emacs-lisp-mode lisp-interaction-mode reb-mode) . rxt-mode))

(use-package wgrep
  :custom
  ;; Save changed buffers immediately when exiting wgrep mode
  (wgrep-auto-save-buffer t)
  :bind
  (:map grep-mode-map
        ("C-c C-p" . wgrep-change-to-wgrep-mode)
        :map occur-mode-map
        ("C-c C-p" . wgrep-change-to-wgrep-mode)))

(use-package rg
  ;; :ensure-system-package
  ;; (rg . ripgrep)
  :custom
  (rg-keymap-prefix (kbd "C-c M-s"))
  :after
  (wgrep-ag)
  :config
  (rg-enable-default-bindings (kbd "C-r"))
  :hook
  (rg-mode . wgrep-ag-setup))

;; (use-package counsel-etags
;;   :custom
;;   ;; TODO: Get this working with Clojure (ctags parses namespaces but
;;   ;; `counsel-etags-find-tag-at-point' doesn't. Wouldn't this be `clojure-mode's
;;   ;; responsibility? I'm pretty sure it keys off of sexp
;;   (tags-revert-without-query t)
;;   ;; Don't warn when TAGS files are large.
;;   (large-file-warning-threshold nil)
;;   :hook
;;   ;; Incrementally update TAGS file when the file is saved.
;;   (prog-mode . (lambda ()
;;                  (add-hook 'after-save-hook
;;                            'counsel-etags-virtual-update-tags 'append 'local)))
;;   :commands
;;   (counsel-etags-find-tag-at-point counsel-etags-scan-code counsel-etags-list-tag))

(use-package company
  :custom
  (company-dabbrev-ignore-case t)
  :hook
  (after-init . global-company-mode)
  :bind
  (([remap dabbrev-expand] . company-complete)
   :map company-active-map
   ;; TODO: The inconsistency between C-n and M-n to select company
   ;; completion in different contexts (e.g `emacs-lisp-mode' and
   ;; `eshell-mode') is aggravating. Not sure about the solution though.
   ;; ("C-n" . company-select-next) ("C-p" . company-select-previous)
   ("RET" . nil)
   ("<return>" . nil)
   ("C-e" . company-complete-selection)
   ("M-." . company-show-location)))

(use-package ivy
  :custom
  (enable-recursive-minibuffers t)
  (ivy-display-style 'fancy)
  :hook
  (after-init . ivy-mode)
  :commands
  (ivy--reset-state ivy-add-actions)
  :bind
  (:map ivy-mode-map
        ("C-c C-r" . ivy-resume)
        :map ivy-minibuffer-map
        ("C-e" . ivy-partial-or-done)))

;; Hydra requirement
(use-package lv)

(use-package ivy-hydra
  :after lv
  :defer 1)

(use-package swiper
  :bind
  (:map ivy-minibuffer-map
        ("C-c C-c" . ivy-toggle-calling)
        ("s-5" . ivy--replace-regexp-entire-buffer)))

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

(use-package counsel
  :custom
  (counsel-find-file-at-point t)
  (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :config
  (ivy-add-actions
   'counsel-M-x
   `(("j" counsel--call-in-other-window-action "other window")))
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
   '(("f"
      ivy--find-file-action
      "find file")
     ("j"
      ivy--switch-buffer-other-window-action
      "other window")
     ("k"
      ivy--kill-buffer-action
      "kill")
     ("r"
      ivy--rename-buffer-action
      "rename")))
  :hook
  (after-init . counsel-mode)
  :bind
  (:map counsel-mode-map
        ("M-x" . counsel-M-x)
        ("C-h C-k" . counsel-descbinds)
        ("s-f" . counsel-grep-or-swiper)
        ("s-F" . counsel-rg)
        ("C-x C-f" . counsel-find-file)
        ("C-x f" . counsel-recentf)
        ("C-x j" . counsel-file-jump)
        ("s-b" . counsel-switch-buffer)
        ("s-B" . counsel-switch-buffer-other-window)
        ("C-h <tab>" . counsel-info-lookup-symbol)
        ("C-h C-a" . counsel-apropos)
        ("C-c u" . counsel-unicode-char)
        ("C-c g" . counsel-git)
        ("C-c j" . counsel-git-grep)
        ("C-c o" . counsel-outline)
        ("M-s-v" . counsel-yank-pop)
        ("M-Y" . counsel-yank-pop)
        ([remap find-file] . counsel-find-file))
  (:map ivy-minibuffer-map
        ("M-y" . ivy-next-line-and-call))
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuffer-history)))

(advice-add 'counsel-rg :around #'counsel-rg-default-directory)

(use-package prescient
  :hook
  (after-init . prescient-persist-mode))

(use-package ivy-prescient
  :hook
  (after-init . ivy-prescient-mode))

(use-package company-prescient
  :hook
  (after-init . company-prescient-mode))

(defvar code-directory (if (file-exists-p "~/code") "~/code" "~")
  "Default code project container directory.")

(use-package projectile
  :custom
  (projectile-keymap-prefix (kbd "C-c p"))
  (projectile-completion-system 'ivy)
  (projectile-project-search-path (list code-directory))
  (projectile-globally-ignored-files '("TAGS" "package-lock.json"))
  (projectile-switch-project-action 'projectile-dired)
  :config
  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm start"
                                    :test "npm test"
                                    :test-suffix ".test")
  :hook
  (projectile-after-switch-project . projectile-load-settings)
  (after-init . projectile-mode)
  :commands
  (projectile-register-project-type)
  :bind
  (:map projectile-mode-map
        ("s-}" . projectile-next-project-buffer)
        ("C-c }" . projectile-next-project-buffer)
        ("s-{" . projectile-previous-project-buffer)
        ("C-c {" . projectile-previous-project-buffer)))

(use-package counsel-projectile
  :custom
  (counsel-projectile-remove-current-buffer t)
  (counsel-projectile-remove-current-project t)
  (compilation-scroll-output t)
  :config
  ;; When switching projects, go straight to dired in the project root.
  (setf (car counsel-projectile-switch-project-action) 4)
  :hook
  (after-init . counsel-projectile-mode)
  :bind
  ("M-s-p" . counsel-projectile-switch-to-buffer)
  ("s-p" . counsel-projectile)
  ("s-P" . counsel-projectile-switch-project)
  ("s-r" . counsel-imenu)
  ("M-s-f" . counsel-projectile-rg))

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'rg)
  :hook
  (prog-mode . dumb-jump-mode)
  ;; dumb-jump shadows some Eshell key bindings, and is not useful there anyway
  (eshell-mode . (lambda () (dumb-jump-mode -1)))
  :bind
  (:map dumb-jump-mode-map
        ("s-j" . dumb-jump-go-prompt)
        ("s-." . dumb-jump-go)
        ("s-J" . dumb-jump-quick-look)))

(use-package flash-thing
  :straight
  (:type git :host github :repo "mnewt/flash-thing")
  :hook
  (after-init . flash-thing-mode))

(use-package spotlight
  :straight
  (:type git :host github :repo "cjp/spotlight.el")
  :commands
  (spotlight spotlight-fast))

(bind-key "s-5" #'replace-regexp-entire-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Version Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; VC follows the link and visits the real file, telling you about it in the
;; echo area.
(setq vc-follow-symlinks t)

;; git config files
(add-to-list 'auto-mode-alist '("\\.git\\(?:config\\|ignore\\).*" . conf-mode))
;; SSH server config files
(add-to-list 'auto-mode-alist '("sshd\?_config" . conf-mode))

(use-package magit
  :custom
  (magit-repository-directories `((,code-directory . 1)))
  (magit-completing-read-function 'ivy-completing-read)
  :commands
  (magit-call-git)
  :bind
  (("C-x g" . magit-status)
   ("C-x C-g" . magit-dispatch)))

(use-package forge
  :after magit)

(use-package git-timemachine
  :bind
  (("C-x t" . git-timemachine)))

(use-package gist
  :commands
  (gist-list))

(use-package diff-hl
  :commands
  (diff-hl-magit-post-refresh diff-hl-mode diff-hl-dired-mode)
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  ((prog-mode markdown-mode) . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode))

(bind-keys
 ("C-c M-l" . git-home-link)
 ("C-c M-u" . git-home-unlink)
 ("C-x G" . projectile-git-ls-files-dired)
 ("C-c ;" . git-add-current-file))

(with-eval-after-load 'dired
  (bind-keys  :map dired-mode-map
              (";" . dired-git-add)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Network and System Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package shr-tag-pre-highlight
  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight))
  :commands
  (shr-tag-pre-highlight))

(use-package w3m
  :commands
  w3m)

;; Automate communication with services, such as nicserv
(with-eval-after-load 'erc
  (require 'erc-services)
  (erc-services-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other File Modes and Formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display nfo files in all their glory
;; https://github.com/wasamasa/dotemacs/blob/master/init.org#display-nfo-files-with-appropriate-code-page)
(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))

;; perl
(setq perl-indent-level tab-width)

;; systemd
(add-to-list 'auto-mode-alist '("\\(?:\\.service\\|\\.timer\\)\\'" . conf-mode))

;; DNS
(add-to-list 'auto-mode-alist '("\\.rpz\\'" . dns-mode))

(setq-default css-indent-offset tab-width)

(use-package markdown-mode
  :mode "\\.md\\|markdown\\'"
  :custom
  (markdown-list-indent-width tab-width)
  (markdown-command "multimarkdown"))

(use-package web-mode
  :mode "\\.html\?\\'"
  :init
  ;; from web-mode FAQ to work with smartparens
  (defun m-web-mode-hook ()
    (setq web-mode-enable-auto-pairing nil))
  (defun sp-web-mode-is-code-context (_id action _context)
    (and (eq action 'insert)
         (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))  :mode
  ("\\.phtml\\'"
   "\\.tpl\\.php\\'"
   "\\.[agj]sp\\'"
   "\\.as[cp]x\\'"
   "\\.erb\\'"
   "\\.mustache\\'"
   "\\.djhtml\\'"
   "\\.html?\\'")
  :custom
  (sgml-basic-offset tab-width)
  (web-mode-markup-indent-offset tab-width)
  (web-mode-css-indent-offset tab-width)
  (web-mode-code-indent-offset tab-width)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-ac-sources-alist '(("css" . (ac-source-css-property))
                               ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  :hook
  (web-mode . m-web-mode-hook))

(use-package company-web
  :commands
  (company-web-html)
  :hook
  (web-mode . (lambda () (set (make-local-variable 'company-backends)
                              (cons 'company-web-html company-backends)))))

(use-package know-your-http-well
  :commands
  (http-header http-method http-relation http-status-code))

(use-package restclient
  :mode "\\.restclient\\'"
  :commands
  (restclient-mode restclient-outline-mode))

(use-package company-restclient
  :hook
  (restclient-mode . (lambda ()
                       (add-to-list 'company-backends 'company-restclient))))

(use-package add-node-modules-path
  :hook
  ((css-mode graphql-mode js2-mode markdown-mode web-mode) . add-node-modules-path))

;; Pulls in `js2-mode' because it is derived from it.
(use-package rjsx-mode
  :mode "\\.jsx?\\'"
  :custom
  (js2-basic-offset tab-width)
  ;; Set tab width for js-mode and json-mode
  (js-indent-level tab-width)
  :hook
  (js2-mode . js2-imenu-extras-mode))

;; Tide is for Typescript but it works great for js/react.
(use-package tide
  :custom
  (tide-format-options `(:indentSize ,tab-width :tabSize ,tab-width))
  (tide-default-mode "JS")
  :commands
  (tide-setup tide-hl-identifier-mode)
  :hook
  ((js2-mode typescript-mode) .
   (lambda ()
     (tide-setup)
     ;; Let tide do the symbol highlighting.
     (symbol-overlay-mode -1)
     (tide-hl-identifier-mode)
     ;; Because we use prettier instead.
     (setq-local flycheck-checkers (remove 'jsx-tide flycheck-checkers)))))

(use-package prettier-js
  ;; :ensure-system-package
  ;; (prettier . "npm i -g prettier")
  :hook
  ((graphql-mode js-mode js2-mode json-mode sass-mode web-mode)  . prettier-js-mode))

(use-package indium
  ;; :ensure-system-package
  ;; (indium . "npm i -g indium")
  :custom
  (indium-chrome-executable "/Applications/Chromium.app/Contents/MacOS/Chromium")
  (indium-chrome-use-temporary-profile nil)
  :commands
  (indium-connect indium-launch))

(use-package json-mode
  ;; :ensure-system-package jq
  :mode "\\.json\\|prettierrc\\'")

(use-package graphql-mode
  :mode "\\(?:\\.g\\(?:\\(?:raph\\)?ql\\)\\)\\'")

;; (use-package genrnc
;;   :custom
;;   (genrnc-user-schemas-directory "~/.emacs.d/schema")
;;   :commands
;;   (genrnc-regist-file))

;; (use-package rnc-mode
;;   :mode "\\.rnc\\'")

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker
  :bind
  ("C-c M-d" . docker))

(use-package docker-tramp
  :defer 2)

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package nginx-mode
  :custom
  (nginx-indent-level tab-width))

(use-package caddyfile-mode
  :mode "\\`Caddyfile\\'")

(use-package yaml-mode
  :mode "\\.ya\?ml\\'")

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package elpy
  ;; :ensure-system-package
  ;; jedi doesn't have an executable and there's not one single path we can look
  ;; across OS and python versions, so just assume it comes with flake8.
  ;; ((flake8 . "pip install jedi flake8")
  ;;  (autopep8 . "pip install autopep8")
  ;;  (yapf . "pip install yapf"))
  :interpreter ("python3?" . python-mode)
  :custom
  (gud-pdb-command-name "python -m pdb")
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  :commands
  (elpy-autopep8-fix-code)
  :hook
  (python-mode . (lambda ()
                   (unless (bound-and-true-p elpy-version) (elpy-enable))
                   (add-hook 'before-save-hook #'elpy-autopep8-fix-code nil t)))
  (python-mode . flycheck-mode)
  :bind
  (:map python-mode-map
        ("s-<return>" . py-execute-expression)))

(use-package company-jedi
  :hook
  (python-mode . (lambda () (set (make-local-variable 'company-backends) '(company-jedi)))))

(use-package enh-ruby-mode
  ;; :ensure-system-package
  ;; (rufo . "gem install rufo")
  :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'")

(use-package inf-ruby
  :hook
  (enh-ruby-mode . inf-ruby-minor-mode)
  (compilation-filter . inf-ruby-auto-enter)
  :commands
  (inf-ruby inf-ruby-console-auto)
  :bind
  (:map inf-ruby-minor-mode-map
        ("s-<return>". ruby-send-last-sexp)
        ("C-M-x" . ruby-send-block)))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package go-mode
  :mode "\\.go\\'")

(use-package company-go
  :hook
  (go-mode . (lambda () (set (make-local-variable 'company-backends) '(company-go)))))

(use-package sass-mode
  :mode "\\(?:s\\(?:[ac]?ss\\)\\)")

(use-package powershell
  :mode "\\.ps1\\'"
  :custom
  (powershell-indent tab-width)
  (powershell-continuation-indent tab-width))

(use-package php-mode
  :mode "\\.php\\'")

(use-package ios-config-mode
  :mode "\\.cfg\\'")

(use-package polymode
  :config
  (define-hostmode poly-rjsx-hostmode
    :mode 'rjsx-mode)
  (define-innermode poly-rjsx-graphql-innermode
    :mode 'graphql-mode
    :head-matcher "graphql[ \t\n]*(?`"
    :tail-matcher "`"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-rjsx-mode
    :hostmode 'poly-web-hostmode
    :innermodes '(poly-rjsx-graphql-innermode))
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
    :innermodes '(poly-web-svg-innermode)))

(use-package poly-markdown)

(use-package fence-edit
  :straight
  (:type git :host github :repo "mnewt/fence-edit.el")
  :config
  (add-multiple-to-list 'fence-edit-blocks
                        '(("---" "---" yaml)
                          ("+++" "+++" toml)
                          ("graphql[ \t\n]*(?`" "`" graphql)
                          ("<svg" "</svg>" nxml t)
                          ("<html" "</html>" web t)
                          ("<div" "</div>" web t)))
  :hook
  ;; Don't shadow the fence-edit binding
  (markdown-mode . (lambda () (bind-key "C-c '" nil markdown-mode-map)))
  :bind
  ("C-c '" . fence-edit-dwim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (file-exists-p "~/.emacs.d/m-private.el")
  (load-file "~/.emacs.d/m-private.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(provide 'init)

;;; init.el ends here
