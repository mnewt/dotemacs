;;; Init.el --- Emacs init file --- -*- lexical-binding: t -*-

;;; Commentary:
;; Single, mostly monolithic Emacs init file. Uses straight.el for package
;; management and use-package for as much package configuration as possible.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Top Level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are good notes on optimizing startup performance:
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast

;; Set Garbage Collection threshold to 1GB, run GC on idle.
(setq gc-cons-threshold 1073741824)
(run-with-idle-timer 20 t (lambda () (garbage-collect)))

;; Stolen from
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(run-with-idle-timer
 5 nil
 (lambda ()
   (nconc file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'file-name-handler-alist-original)))

(setq load-prefer-newer t)

(with-eval-after-load 'gnutls
  (setq gnutls-verify-error t))

(with-eval-after-load 'nsm
  (setq network-security-level 'high))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable package.el initialization.
(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my initl!
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

(straight-use-package 'use-package)
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)

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

(use-package use-package-ensure-system-package)

;; (use-package benchmark-init
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cl is assumed to be present in this config and some packages too.
(require 'cl)
(require 'seq)

;; All external packages and many built in ones are configured using use-package.
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Packages go here.
(add-to-list 'load-path "~/.emacs.d/elisp/")

(defun add-multiple-to-list (list items)
  "Run `add-to-list' for all ITEMS in the LIST."
  (seq-do (apply-partially #'add-to-list list) items))

(use-package dash)

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

(use-package doom-themes)

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

(defvar theme-selected-window (frame-selected-window)
  "Selected window.")

(defun theme-set-selected-window ()
  "Set the variable `theme-selected-window' appropriately."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq theme-selected-window (frame-selected-window))
    (force-mode-line-update)))

(defun theme-unset-selected-window ()
  "Unset the variable `theme-selected-window' and update the mode line."
  (setq theme-selected-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook 'theme-set-selected-window)

;; focus-in-hook was introduced in emacs v24.4.
;; Gets evaluated in the last frame's environment.
(add-hook 'focus-in-hook 'theme-set-selected-window)

;; focus-out-hook was introduced in emacs v24.4.
(add-hook 'focus-out-hook 'theme-unset-selected-window)

;; Executes after the window manager requests that the user's events
;; be directed to a different frame.
(defadvice handle-switch-frame (after theme-handle-switch-frame activate)
  "Call `theme-set-selected-window'."
  (theme-set-selected-window))

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

(defun get-attr (propname attribute name)
  "Get the ATTRIBUTE identified by NAME from PROPNAME."
  (let ((name (if (stringp name) (intern name) name)))
    (cl-some (lambda (e) (when (and (eq attribute (car e)) (eq name (cadr e)))
                           (cadddr e)))
             (get (car custom-enabled-themes) 'theme-settings))))

(defun theme-get-attr (attribute name)
  "Get the ATTRIBUTE identified by NAME from the current theme settings.

Example usage

    \(plist-get
      \(face-spec-choose \(theme-get-attr 'theme-face 'default))
      \:background)"
  (get-attr 'theme-settings attribute name))

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
         (highlight-spec (face-spec-choose (theme-get-face 'highlight)))
         (outline-1-spec (face-spec-choose (theme-get-face 'outline-1)))
         (active-bg (plist-get default-spec :background))
         (active-fg (plist-get default-spec :foreground))
         (inactive-bg (doom-blend active-bg active-fg 0.9))
         (inactive-fg (doom-blend active-bg active-fg 0.4))
         (highlight-fg (plist-get outline-1-spec :foreground)))
    `((default ((t :background ,inactive-bg)))
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
  :config
  (window-highlight-mode 1))

(defvar fiat-state 'nox
  "Whether we let there be light or dark.")

(defun fiat--set-os-dark-mode (p)
  ;; Change directory in case we are in a tramp session.
  (let ((default-directory "~")
        (p (if p "true" "false")))
    (shell-command (format "osascript -e '
tell application \"System Events\"
  tell appearance preferences to set dark mode to %s
end tell'" p))))

(defun fiat ()
  "Let there be the opposite of whatever came before."
  (interactive)
  (if (eq fiat-state 'nox) (fiat-lux) (fiat-nox)))

(defun fiat-lux ()
  "Switch Emacs and OS to light mode."
  (interactive)
  (setq fiat-state 'lux)
  (theme-activate 'solarized-light)
  (fiat--set-os-dark-mode nil))

(defun fiat-nox ()
  "Switch Emacs and OS to dark mode."
  (interactive)
  (setq fiat-state 'nox)
  (theme-activate 'doom-dracula)
  (fiat--set-os-dark-mode t))

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
          (message "%s" (prin1-to-string `(setenv ,var ,value)))
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
    (message "New path: %s" new-path)
    (setq exec-path new-path)))

(source-sh "~/.env")
(source-sh "~/.bin/start-ssh-agent")
(set-path)

(defun expand-environment-variable ()
  "Insert contents of an envionment variable at point."
  (interactive)
  (insert (getenv (read-envvar-name "Insert Environment Variable: "))))

(bind-key "C-c C-v" 'expand-environment-variable)

(add-hook 'after-init-hook (lambda ())
          (require 'server)
          (unless (server-running-p) (server-start)))

(use-package restart-emacs
  :commands
  (restart-emacs))

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

(defun touch (cmd)
  "Run `touch CMD' in `default-directory'."
  (interactive
   (list (read-shell-command "Run touch (like this): "
                             "touch "
                             'touch-history
                             "touch ")))
  (shell-command cmd))

(defvar os-open-file-executable nil)

(defun os-open-file (file)
  "Open FILE using the operating system's GUI file opener."
  (interactive)
  (message "Opening %s..." file)
  (call-process os-open-file-executable nil 0 nil file))

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
        trash-directory "~/.Trash")

  (use-package reveal-in-osx-finder
    :config
    (defalias 'os-reveal-file #'reveal-in-osx-finder)))

(defun config-windows ()
  "Configure Emacs for Windows."
  (menu-bar-mode -1)
  (setq w32-pass-lwindow-to-system nil
        w32-lwindow-modifier 'super
        w32-pass-rwindow-to-system nil
        w32-rwindow-modifier 'super
        os-open-file-executable "explorer")
  (set-face-font 'default "Lucida Console-12")

  (defun reveal-in-windows-explorer (&optional file)
    "Reveal the current FILE in the operating system's file manager."
    (interactive)
    (unless file (setq file buffer-file-name))
    (os-open-file (concat "/select," (dired-replace-in-string "/" "\\" file))))

  (defalias 'os-reveal-file #'reveal-in-windows-explorer))

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
(desktop-save-mode 1)
(add-to-list 'desktop-globals-to-save 'kill-ring)
(add-to-list 'desktop-globals-to-save 'theme-current-theme)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show line in the original buffer from occur mode
(setq list-matching-lines-jump-to-current-line t)

(defun next-line-4 ()
  "Scroll 4 lines down."
  (interactive)
  (forward-line 4))

(defun previous-line-4 ()
  "Scroll 4 lines up."
  (interactive)
  (forward-line -4))

(bind-keys
 ("C-S-p" . previous-line-4)
 ("C-S-n" . next-line-4)
 ("H-p" . "\C-u1\M-v")
 ("H-n" . "\C-u1\C-v"))

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
 ("s-i" . os-reveal-file)
 ("C-c i" . os-reveal-file)
 ("s-<return>" . eval-last-sexp)
 ("s-RET" . eval-last-sexp))

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
  "Return a list of all major modes which are associated with a
  magic string or file extension."
  (delete-dups (mapcar #'cdr (append magic-mode-alist
                                     auto-mode-alist
                                     magic-fallback-mode-alist))))

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

;; Quick switch buffers
(bind-keys ("C-x C-b" . ibuffer)
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

           ;; Navigating with mark
           ("M-s-," . pop-to-mark-command)
           ("C-c ," . pop-to-mark-command)
           ("s-," . pop-global-mark)
           ("C-c C-," . pop-global-mark))

(use-package winner
  :init
  (winner-mode)
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
  :custom
  (eyebrowse-new-workspace t)
  (eyebrowse-mode-line-separator " ")
  :config
  (eyebrowse-mode t)
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

(defun new-scratch-buffer ()
  "Create or go to a scratch buffer in the current mode.

If ARG is provided then prompt for the buffer's mode. Try these
  things in succession\:

1. Select an existing window containing the scratch buffer.
2. Switch to an existing scratch buffer.
3. Create a new scratch buffer and switch to it."
  (interactive)
  (let* ((mode (if current-prefix-arg
                   (intern (ivy-read "New scratch buffer with mode: "
                                     (list-major-modes)
                                     :history 'new-scratch-buffer-history
                                     :caller 'new-scratch-buffer))
                 ;; :initial-input (car new-scratch-buffer-history)))
                 major-mode))
         (name (format "<%s>" (symbol-name mode)))
         (win (get-buffer-window name)))
    (cond
     (win (select-window win))
     (t (switch-to-buffer (get-buffer-create name))
        (setq buffer-file-name name)
        (funcall mode)))))

(defun new-scratch-buffer-other-window ()
  "Create or go to a scratch buffer in ther current mode.

For for details see `new-scratch-buffer'."
  (interactive)
  (switch-to-buffer-other-window (current-buffer))
  (new-scratch-buffer))

(bind-keys ("s-n" . new-scratch-buffer)
           ("s-N" . new-scratch-buffer-other-window)
           ("C-c C-n" . new-scratch-buffer)
           ("C-c M-n" . new-scratch-buffer-other-window))

;; Try to re-use help buffers of different sorts
(setq display-buffer-alist
      `((,(rx bos
              (or "*Apropos*" "*eww*" "*Help*" "*helpful" "*info*" "*Summary*")
              (0+ not-newline))
         (display-buffer-reuse-mode-window display-buffer-pop-up-window)
         (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))

;; kill buffer and window
(defun kill-other-buffer-and-window ()
  "Kill the buffer in the other window."
  (interactive)
  (select-window (next-window))
  (kill-buffer-and-window))

(bind-keys ("M-s-w" . kill-buffer-and-window)
           ("M-s-W" . kill-other-buffer-and-window))

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

(bind-keys ("M-s-<up>" . shrink-window)
           ("M-s-<down>" . enlarge-window)
           ("M-s-<left>" . shrink-window-horizontally)
           ("M-s-<right>" . enlarge-window-horizontally)
           :map ctl-x-4-map
           ("t" . toggle-window-split))

;; Tags
(bind-keys ("s-R" . xref-find-definitions-other-window)
           ("C-c M-r" . xref-find-definitions-other-window))

(defun find-file-at-point-with-line (&optional filename)
  "Open FILENAME at point and move point to line specified next to file name."
  (interactive)
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

(bind-key "C-c C-f" #'find-file-at-point-with-line)

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

(advice-add 'server-visit-files :around #'wrap-colon-notation)

;; Just set up 3 windows, no fancy frames or whatever
(with-eval-after-load 'ediff
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

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

(bind-key [remap goto-line] #'goto-line-with-feedback)

(use-package evil
  :bind
  ("s-ESC" . evil-mode))

(use-package goto-addr
  :hook
  ((compilation-mode text-mode eshell-mode shell-mode) . goto-address-mode)
  (prog-mode . goto-address-prog-mode)
  :bind
  (:map goto-address-highlight-keymap
        ("C-c C-o" . goto-address-at-point)))

(defun outline-show-current-sublevel ()
  "Show only the current top level section."
  (interactive)
  (unless outline-minor-mode
    (outline-minor-mode t))
  (outline-hide-sublevels 1)
  (outline-show-subtree))

(defun outline-subtree-previous ()
  "Go to and expand previous sublevel."
  (interactive)
  (unless outline-minor-mode
    (outline-minor-mode t))
  (outline-hide-sublevels 1)
  (outline-previous-visible-heading 1)
  (outline-show-subtree))

(defun outline-subtree-next ()
  "Go to and expand previous sublevel."
  (interactive)
  (unless outline-minor-mode
    (outline-minor-mode t))
  (outline-hide-sublevels 1)
  (outline-next-visible-heading 1)
  (outline-show-subtree))

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
  (prog-mode . symbol-overlay-mode))

(use-package rainbow-mode
  :hook
  ((css-mode emacs-lisp-mode sass-mode) . rainbow-mode))

(use-package crux
  :bind
  ("C-c C-j" . crux-eval-and-replace)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("C-c R" . crux-rename-file-and-buffer)
  ("C-c k" . crux-kill-other-buffers)
  ("C-M-X" . crux-indent-defun)
  ("C-c I" . (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
  ("C-c S" . crux-find-shell-init-file)
  ("C-<backspace>" . crux-kill-line-backwards))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help and Documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq suggest-key-bindings 5
      ;; Select help window so it's easy to quit it with `q'
      help-window-select t)

(use-package help-at-pt
  :custom
  (help-at-pt-display-when-idle t)
  :config
  (help-at-pt-set-timer))

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
  :demand t
  :config
  (which-key-mode t)
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
  :ensure-system-package
  (eg . "pip install eg")
  :straight
  (:type git :host github :repo "mnewt/eg.el")
  :bind
  ("C-h e" . eg))

(defun dash-docs-installed-docsets ()
  "Return a list of the currently installed docsets."
  (mapcar (lambda (f) (string-trim-right f ".docset"))
          (directory-files dash-docs-docsets-path nil "[^.]*\.docset")))

(defun dash-docs-update-all-docsets ()
  "Update all docsets."
  (interactive)
  (seq-do (lambda (d) (when (memq d (dash-docs-official-docsets)) (dash-docs-install-docset)))
          (dash-docs-installed-docsets)))

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

;; Replace `delete-horizontal-space' with the more useful `cycle-spacing'.
(bind-key "M-\\" #'cycle-spacing)

;; Continue comment on next line (default binding is "C-M-j")
(bind-key "M-RET" #'indent-new-comment-line)

(defun configure-auto-fill-mode ()
  "Automatically fill comments.
Wraps on `fill-column' columns."
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'prog-mode-hook #'configure-auto-fill-mode)

(defun move-line-or-region-to-other-window ()
  "Kill region (if active) or the current line then yank at point
in the other window."
  (interactive)
  (kill-line-or-region)
  (other-window 1)
  (yank)
  (newline)
  (other-window -1))

(defun copy-line-or-region-to-other-window ()
  "Copy region (if active) or the current line to point in the
other window."
  (interactive)
  (copy-line-or-region)
  (other-window 1)
  (yank)
  (newline)
  (other-window -1))

(bind-keys ("s-C" . copy-line-or-region-to-other-window)
           ("s-X" . move-line-or-region-to-other-window))

;; sh-mode
(use-package sh-script
  :ensure-system-package
  (shfmt . "brew install shfmt")
  :custom
  (sh-basic-offset tab-width)
  (sh-indentation tab-width)
  ;; Tell `executable-set-magic' to insert #!/usr/bin/env interpreter
  (executable-prefix-env t))

;; Make a shell script executable automatically on save
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(defun maybe-reset-major-mode ()
  "Reset the buffer's `major-mode' if a different mode seems like a better fit.
Mostly useful as a `before-save-hook', to guess mode when saving a
new file for the first time.
https://github.com/NateEag/.emacs.d/blob/9d4a2ec9b5c22fca3c80783a24323388fe1d1647/init.el#L137"
  (when (and
         ;; The buffer's visited file does not exist.
         (eq (file-exists-p (buffer-file-name)) nil)
         (eq major-mode 'fundamental-mode))
    (normal-mode)))

(add-hook 'before-save-hook #'maybe-reset-major-mode)

;; dw (https://gitlab.com/mnewt/dw)
(add-to-list 'auto-mode-alist '("\\DWfile.*\\'" . sh-mode))

;; Automatically indent after RET
(electric-indent-mode +1)

;; http://whattheemacsd.com/key-bindings.el-03.html
(defun delete-indentation-forward ()
  "Like `delete-indentation', but in the opposite direction.
Bring the line below point up to the current line."
  (interactive)
  (join-line -1))

(bind-key "C-^" #'delete-indentation-forward)

(use-package redo+
  :straight
  (:type git :host github :repo "clemera/undo-redo")
  :bind
  ("s-z" . undo-modern)
  ("C-z" . undo-modern)
  ("s-Z" . redo)
  ("s-y" . redo))

(use-package undohist
  :config
  (undohist-initialize)
  (advice-add 'undohist-recover-1
              :around
              (lambda (f) (cl-flet ((yes-or-no-p (_) t))))))

(use-package undo-propose
  :straight
  (:type git :host github :repo "jackkamm/undo-propose-el")
  :bind
  (("C-s-z" . undo-propose)
   :map undo-propose-mode-map
   ([remap undo-modern] . undo-propose-undo)
   ([remap redo] . undo-propose-undo)))

(use-package volatile-highlights
  :config
  (vhl/define-extension 'undo 'redo 'undo-modern)
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)
  (volatile-highlights-mode t))

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
  :defer 2
  :custom
  (yas-verbosity 1)
  :config
  (yas-global-mode 1)
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

(defun indent-buffer-or-region (&optional arg)
  "Indent the region if one is active, otherwise format the buffer.
Some modes have special treatment."
  (interactive "P")
  (when (sp-point-in-string-or-comment)
    (fill-paragraph arg))
  (call-interactively #'crux-cleanup-buffer-or-region)
  (if (use-region-p)
      (progn
        (indent-region (region-beginning) (region-end))
        (message "Region indented."))
    (progn
      (format-all-buffer)
      (message "Buffer formatted."))))

(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-buffer-or-region (region-beginning) (region-end))))

(bind-keys ("C-M-\\" . indent-buffer-or-region)
           ("C-\\" . indent-defun))

(use-package darkroom
  :bind
  ("C-c C-d" . darkroom-mode)
  :commands
  (darkroom-mode darkroom-tentative-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lisp, S-Expressions, Parentheses, Brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sp-sh-post-handler (_id action _context)
  "Bash post handler ID, ACTION, CONTEXT."
  (-let (((&plist :arg arg :enc enc) sp-handler-context))
    (when (equal action 'barf-backward)
      (sp-ruby-delete-indentation 1)
      (indent-according-to-mode)
      (save-excursion
        (sp-backward-sexp)              ; move to begining of current sexp
        (sp-backward-sexp arg)
        (sp-ruby-maybe-one-space)))

    (when (equal action 'barf-forward)
      (sp-get enc
        (let ((beg-line (line-number-at-pos :beg-in))))))
    (sp-forward-sexp arg)
    (sp-ruby-maybe-one-space)
    (when (not (= (line-number-at-pos) beg-line))
      (sp-ruby-delete-indentation -1))
    (indent-according-to-mode)))

(defun sp-sh-block-post-handler (id action context)
  "Handler for bash block insertions.
ID, ACTION, CONTEXT."
  (when (equal action 'insert)
    (save-excursion
      (newline)
      (indent-according-to-mode))
    (indent-according-to-mode))
  (sp-sh-post-handler id action context))

(defun sp-sh-pre-handler (_id action _context)
  "Handler for bash slurp and barf.
ID, ACTION, CONTEXT."
  (let ((enc (plist-get sp-handler-context :enc)))
    (sp-get enc
      (let ((beg-line (line-number-at-pos :beg-in))))
      (end-line (line-number-at-pos :end-in)

                (when (equal action 'slurp-backward))))
    (save-excursion
      (sp-forward-sexp)
      (when (looking-at-p ";") (forward-char))
      (sp-ruby-maybe-one-space)
      (when (not (= (line-number-at-pos) end-line))
        (sp-ruby-delete-indentation -1)))
    (while (thing-at-point-looking-at "\\.[[:blank:]\n]*")
      (sp-backward-sexp))
    (when (looking-back "[@$:&?!]")
      (backward-char)
      (when (looking-back "[@&:]")
        (backward-char)))
    (just-one-space)
    (save-excursion
      (if (= (line-number-at-pos) end-line
             (insert " "))
          (newline

           (when (equal action 'barf-backward)))))
    ;; Barf whole method chains
    (while (thing-at-point-looking-at "[(.:[][\n[:blank:]]*")
      (sp-forward-sexp))
    (if (looking-at-p " *$")
        (newline)
      (save-excursion (newline)

                      (when (equal action 'slurp-forward))))
    (save-excursion
      (sp-backward-sexp)
      (when (looking-back "\." nil) (backward-char))
      (sp-ruby-maybe-one-space)
      (when (not (= (line-number-at-pos) beg-line))
        (if (thing-at-point-looking-at "\\.[[:blank:]\n]*")))
      (progn
        (forward-symbol -1)
        (sp-ruby-delete-indentation -1
                                    (sp-ruby-delete-indentation))))
    (while (looking-at-p "::") (sp-forward-symbol))
    (when (looking-at-p "[?!;]") (forward-char))
    (if (= (line-number-at-pos) beg-line)
        (insert " ")
      (newline

       (when (equal action 'barf-forward))))
    (when (looking-back "\\." nil) (backward-char))
    (while (looking-back "::" nil) (sp-backward-symbol))
    (if (= (line-number-at-pos) end-line)
        (insert " ")
      (if (looking-back "^[[:blank:]]*" nil
                        (save-excursion (newline)))
          (newline)))))

(defun sp-backward-slurp-into-previous-sexp ()
  "Add the sexp at point into the preceeding list."
  (interactive)
  (save-excursion
    (sp-down-sexp)
    (sp-backward-symbol)
    (sp-forward-slurp-sexp)))

;; See https://github.com/Fuco1/smartparens/issues/80
(defun sp-create-newline-and-enter-sexp (&rest _)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

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
  :config
  (bind-key [remap kill-line] #'sp-kill-hybrid-sexp smartparens-mode-map
            (apply #'derived-mode-p sp-lisp-modes))
  (sp-with-modes
      '(c-mode c++-mode css-mode graphql-mode javascript-mode js2-mode json-mode objc-mode
               python-mode java-mode sh-mode web-mode)
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
                   :post-handlers '(sp-sh-block-post-handler)))
  (smartparens-global-mode)
  :hook
  (smartparens-mode . (lambda ()
                        (require 'smartparens-config)
                        (turn-on-show-smartparens-mode)))
  (prog-mode . turn-on-smartparens-mode)
  (clojure-mode . (lambda () (require 'smartparens-clojure)))
  ((ruby-mode enh-ruby-mode) . (lambda () (require 'smartparens-ruby)))
  ((javascript-mode js2-mode json-mode) . (lambda () (require 'smartparens-javascript)))
  (lua-mode . (lambda () (require 'smartparens-lua)))
  (markdown-mode . (lambda () (require 'smartparens-markdown)))
  (org-mode . (lambda () (require 'smartparens-org)))
  (python-mode . (lambda () (require 'smartparens-python)))
  (text-mode . (lambda () (require 'smartparens-text)))
  (web-mode . (lambda () (require 'smartparens-html))))

(add-to-list 'auto-mode-alist '("Cask\\'" emacs-lisp-mode))

(with-eval-after-load 'elisp-mode (require 'm-lisp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shell, Terminal, SSH, Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'dired (require 'm-dired))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(with-eval-after-load 'tramp (require 'm-shell-common))

(with-eval-after-load 'shell (require 'm-shell-common))

(with-eval-after-load 'term (require 'm-shell-common))

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
  (with-eval-after-load 'sh
    (bind-keys :map sh-mode-map
               ("s-<ret>" . eshell-send-current-line)))
  (require 'm-eshell)
  :hook
  ((eshell-mode . eshell/init)
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
  :custom
  (password-cache-expiry nil)
  :config
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  ;; TODO: Don't know how to get pinentry to work with Windows. Maybe a TCP socket?
  (unless (eq system-type 'windows-nt)
    (pinentry-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mount
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes, Journal, and Documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'org (require 'm-org))

;; visual-line-mode
;; Don't shadow mwim and org-mode bindings
(bind-key [remap move-beginning-of-line] nil visual-line-mode-map)
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

(defvar mode-line-format-backup nil
  "Backup of `mode-line-format'.")

(defun hide-mode-line ()
  "Hide the mode line."
  (interactive)
  (setq mode-line-format-backup mode-line-format)
  (setq-default mode-line-format nil))

(defun show-mode-line ()
  "Show the mode line."
  (interactive)
  (setq-default mode-line-format mode-line-format-backup))

;; (use-package darkroom-mode
;;   :commands
;;   (darkroom-mode darkroom-tentative-mode))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  :bind
  (:map pdf-view-mode-map
        ("s-f" . isearch-forward)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Log files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vlf
  :custom
  (vlf-application 'dont-ask)
  :config
  (require 'vlf-setup))

;; (use-package logview)

(defun tail-file (file)
  "Run `tail -f' on FILE.
Tries to find a file at point."
  (interactive (list (completing-read "Tail file: "
                                      'read-file-name-internal
                                      'file-exists-p t nil 'file-name-history
                                      (thing-at-point 'filename))))
  (async-shell-command (concat "tail -f " file)))

(bind-key "F" #'tail-file dired-mode-map)

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
  :ensure-system-package
  (rg . ripgrep)
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
  (:map prog-mode-map
        ("M-/" . company-complete)
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
  :config
  (ivy-mode 1)
  :bind
  (:map ivy-mode-map
        ("C-c C-r" . ivy-resume)
        :map ivy-minibuffer-map
        ("C-e" . ivy-partial-or-done)))

(use-package lv)

(use-package ivy-hydra
  :after lv
  :defer 1)

(defun replace-regexp-entire-buffer (pattern replacement)
  "Immediately replace PATTERN with REPLACEMENT throughout the buffer."
  (interactive
   (let ((args (query-replace-read-args "Replace in entire buffer" t)))
     (setcdr (cdr args) nil)    ; remove third value returned from query---args
     args))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match replacement))))

(defun ivy--replace-regexp-entire-buffer (replacement)
  "Replace the currently input via regexp with REPLACEMENT."
  (interactive (list (read-from-minibuffer (concat "Replace all occurrences of `" ivy--old-re "\' in entire buffer: "))))
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (replace-regexp-entire-buffer ivy--old-text replacement))
  (ivy-done)
  ;; * TODO: How to make this the last message displayed?
  (message (concat "Replaced `" ivy--old-text "\' with `" replacement"\' across entire buffer.")))

(use-package swiper
  :bind
  (("s-5" . replace-regexp-entire-buffer)
   :map ivy-minibuffer-map
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

(defun counsel--call-in-other-window-action (x)
  "Call the command represented by string X after switching to
the other window."
  (switch-to-buffer-other-window (current-buffer)
                                 (call-interactively (intern x))))

(use-package counsel
  :custom
  (counsel-find-file-at-point t)
  (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :config
  (counsel-mode)
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
  :bind
  (:map counsel-mode-map
        ("C-h C-k" . counsel-descbinds)
        ("s-F" . counsel-rg)
        ("s-f" . counsel-grep-or-swiper)
        ("M-x" . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("C-x f" . counsel-recentf)
        ("C-x j" . counsel-file-jump)
        ("s-b" . counsel-switch-buffer)
        ("s-B" . counsel-buffer-other-window)
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

(defun counsel-rg-default-directory (f &rest args)
  "Call F (`counsel-rg') with ARGS from `default-directory'.

It seems like `counsel-rg' should call itself from
`default-directory' without assistance but in my experience it
looks for a project root directory instead. If we want to search
from the project root, we can use `counsel-projectile-rg'."
  (let ((initial-input (car args))
        (initial-directory (or (cadr args) default-directory))
        (extra-rg-args (caddr args))
        (rg-prompt (or (cadddr args) (format "(%s) rg: " default-directory))))
    (funcall f initial-input initial-directory extra-rg-args rg-prompt)))

(advice-add 'counsel-rg :around #'counsel-rg-default-directory)

(use-package ivy-dired-history
  :config
  (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)
  :bind
  (:map dired-mode-map
        ("," . dired)))

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :config
  (company-prescient-mode))

(defun projectile-load-settings (&optional file)
  "Load project elisp settings from FILE.
Look in active project root directory, or if in the case of
  undefined root directory, file is otherwise path resolvable.

https://github.com/jfeltz/projectile-load-settings/blob/master/projectile-load-settings.el"
  (interactive)
  (let ((p (expand-file-name (or file "config.el") (projectile-project-root))))
    (when (file-exists-p p)
      (load p)
      (message "%s" (concat "Loaded project settings from: " p)))))

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
  (projectile-mode +1)
  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm start"
                                    :test "npm test"
                                    :test-suffix ".test")
  :hook
  (projectile-after-switch-project . projectile-load-settings)
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
  (counsel-projectile-mode)
  ;; When switching projects, go straight to dired in the project root.
  (setf (car counsel-projectile-switch-project-action) 4)
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

(defun git-add-current-file (file)
  "Run `git add' on the current buffer file name."
  (interactive (list (buffer-file-name)))
  (shell-command (format "git add '%s'" file)))

(defun dired-git-add ()
  "Run `git add' on the selected files in a dired buffer."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (message "> git add %s" files)
    (dired-do-shell-command "git add" nil files)))

(bind-keys ("C-c ;" . git-add-current-file)
           :map dired-mode-map
           (";" . dired-git-add))

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
  ;; Configure projectile back to default
  (if (boundp 'projectile-git-command)
      (setq projectile-git-command "git ls-files -zco --exclude-standard"))
  ;; This does `git config --unset core.worktree'.  We don't actually
  ;; have to do this and not doing it would have some advantages, but
  ;; might be confusing.
  ;; (magit-set nil "core.worktree")
  ;; This causes an error if this actually is a directory, which is
  ;; a good thing, it saves us from having to do this explicitly :-)
  (delete-file (expand-file-name ".git" worktree)))

(setq git-home-repo-dir
      (expand-file-name "repos" (or (getenv "XDG_CONFIG_HOME") "~/.config")))

(defun git-home-link (repo)
  "Interactively link a git REPO's worktree to $HOME."
  (interactive (list (completing-read "Link git home repository: "
                                      (directory-files git-home-repo-dir nil "^[^.]")
                                      nil t)))
  (setq repo (expand-file-name repo git-home-repo-dir))
  ;; "Fix" repositories that were created with --bare.
  ;; (let ((default-directory (file-name-as-directory repo)))
  ;;   (magit-set "false" "core.bare"))
  ;; Regular link.
  (git-worktree-link repo (getenv "HOME"))
  (message "Linked repo at %s" repo))

(defun git-home-unlink ()
  "Unlink the current git repo's worktree from $HOME."
  (interactive)
  (let ((f (expand-file-name ".git" (getenv "HOME"))))
    (git-worktree-unlink (getenv "HOME"))
    (message "Unlinked repo at %s" f)))

(bind-keys
 ("C-c M-l" . git-home-link)
 ("C-c M-u" . git-home-unlink))

(defun projectile-git-ls-files (&optional dir)
  "List of the tracked files in the git repo, specified by DIR."
  (cd (or dir (projectile-project-root)))
  (-filter #'nil-blank-string
           (split-string (shell-command-to-string "git ls-files") "\n")))

(defun projectile-git-ls-files-dired (&optional dir)
  "Dired list of the tracked files in the git repo, specified by DIR."
  (interactive)
  (let ((dir (or dir (projectile-project-root))))
    (dired (cons dir (projectile-git-ls-files dir)))
    (rename-buffer (format "*git ls-files %s*" dir))))

(bind-key "C-x G" #'projectile-git-ls-files-dired)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Network and System Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package eww
;;   :commands
;;   (eww))

(use-package shr-tag-pre-highlight
  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight))
  :commands
  (shr-tag-pre-highlight))

(use-package w3m
  :commands
  (w3m w3m-weather))

(defun public-ip ()
  "Display the local host's apparent public IP address."
  (interactive)
  (message
   (with-current-buffer (url-retrieve-synchronously "https://diagnostic.opendns.com/myip")
     (goto-char (point-min))
     (re-search-forward "^$")
     (delete-char 1)
     (delete-region (point) (point-min))
     (buffer-string))))

(defun df ()
  "Display the local host's disk usage in human readable form."
  (interactive)
  (print (shell-command-to-string "df -h")))

(defun dis (hostname)
  "Resolve a HOSTNAME to its IP address."
  (interactive "MHostname: ")
  (message (shell-command-to-string
            (concat "drill "
                    hostname
                    " | awk '/;; ANSWER SECTION:/{flag=1;next}/;;/{flag=0}flag'"))))

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

;; XML
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

(defun toggle-sp-newline ()
  "Toggle whether `RET' is bound to `newline' or `sp-newline'.

This is because under certain conditions `sp-newline' can do bad
things."
  (interactive)
  (let* ((f (key-binding (kbd "RET")))
         (newf (if (eq f 'sp-newline) #'newline #'sp-newline)))
    (bind-key "RET" newf smartparens-mode-map)
    (message "<RET> now invokes to %s" newf)))

(bind-key "C-c C-<return>" #'toggle-sp-newline)

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
  :hook
  ((js2-mode typescript-mode) .
   (lambda ()
     (tide-setup)
     (tide-hl-identifier-mode)
     ;; Because we use prettier.
     (setq flycheck-checkers (remove 'jsx-tide flycheck-checkers)))))

(use-package prettier-js
  :ensure-system-package
  (prettier . "npm i -g prettier")
  :hook
  ((graphql-mode js-mode js2-mode json-mode sass-mode web-mode)  . prettier-js-mode))

(use-package indium
  :ensure-system-package
  (indium . "npm i -g indium")
  :custom
  (indium-chrome-executable "/Applications/Chromium.app/Contents/MacOS/Chromium")
  (indium-chrome-use-temporary-profile nil)
  :commands
  (indium-connect indium-launch))

(use-package json-mode
  :ensure-system-package
  jq
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
  :ensure-system-package
  ;; jedi doesn't have an executable and there's not one single path we can look
  ;; across OS and python versions, so just assume it comes with flake8.
  ((flake8 . "pip install jedi flake8")
   (autopep8 . "pip install autopep8")
   (yapf . "pip install yapf"))
  :interpreter ("python3?" . python-mode)
  :custom
  (gud-pdb-command-name "python -m pdb")
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
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
  :ensure-system-package
  (rufo "gem install rufo")
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
  :ensure-system-package t
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

(defun advice-delete-other-windows (&rest _)
  "Advice that will delete other windows."
  (delete-other-windows))

(advice-add 'wttrin :before #'advice-delete-other-windows)

(use-package wttrin
  :custom
  (wttrin-default-cities '("Albany CA"
                           "San Francisco CA"
                           "Austin TX"
                           "Eugene OR"
                           "Truckee CA"
                           "Moon"))
  (wttrin-default-accept-language '("Accept-Language" . "en-US"))
  :bind
  ("C-c M-w" . wttrin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load private stuff if it exists
(require 'm-private nil 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(provide 'init)

;;; init.el ends here
