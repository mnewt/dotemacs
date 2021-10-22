;;; init.el --- Emacs init -*- lexical-binding: t; -*-

;;; Commentary:

;; It's an Emacs init file.  It relies on heavily on `use-package' for
;; organization and delayed loading and `straight' for packaage managment.

;;; Code:

;;;; Start

;; Things that run at the very beginning of Emacs startup

;; Ensure `early-init.el' runs even on older Emacs versions.
(eval-when-compile
  (when (version< emacs-version "27")
    (load "~/.emacs.d/early-init.el")))

;;;;; User variables

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defvar code-directory "~/code"
  "Default directory for storing programming projects.")

;; For `esup'.
;; (setq vc-follow-symlinks t)


;;;;; Environment Variables

;; PATH and LIBRARY_PATH need to be defined before any packages get loaded. This
;; is so that `comp' (Native Compilation) can find libgccjit and friends.  As
;; soon as `straight' loads a package (including itself), it tells `comp' to
;; start compiling it.  So, we add a caching wrapper to `exec-path-from-shell'
;; (`env-cache') so under most startups we don't even have to load it.  When we
;; do have to load it, it's actually sitting in the lisp directory, not loaded
;; by `straight'.

(defvar env-cache-vars
  '("GOPATH" "LANG" "LC_COLLATE" "LC_CTYPE" "LC_MESSAGES" "LIBRARY_PATH"
    "MANPATH" "PATH" "PKG_CONFIG_PATH" "SHELL" "TEMPDIR" "USER")
  "Variables to copy from the shell environment.")

(defvar env-cache-file
  (expand-file-name "var/env" user-emacs-directory)
  "File in which to save the environment variable cache.")

(defun env-cache-save-to-file (&optional vars)
  "Save VARS to `env-cache-file'.

If VARS is not specified, use `env-cache-vars'."
  (make-directory (file-name-directory env-cache-file) t)
  (with-temp-file env-cache-file
    (prin1 (mapcar (lambda (var) (cons var (getenv var)))
                   (or vars env-cache-vars))
           (current-buffer))))

(defun env-cache-refresh ()
  "Load shell, copy and cache environment variables."
  (interactive)
  ;; NOTE `exec-path-from-shell' is kept in the `lisp/' directory.  It's not
  ;; managed by `straight'.  This is because we may need it before we load
  ;; `straight'.  So we download it the old-fashioned way.
  (require 'exec-path-from-shell)
  (message "Loading %s to copy environment variables into Emacs..."
           (getenv "SHELL"))
  (setq exec-path-from-shell-arguments nil
        exec-path-from-shell-check-startup-files nil)
  ;; When bash is invoked with no arguments (i.e. non-login, non-interactive),
  ;; it only sources $BASH_ENV.
  (setenv "BASH_ENV" (expand-file-name ".bashrc" (getenv "HOME")))
  (exec-path-from-shell-copy-envs env-cache-vars)
  (when (eq system-type 'windows-nt)
    (exec-path-from-shell-setenv
     "PATH"
     (concat (getenv "PATH") ";C:/bin;C:/Program Files/Emacs/bin")))
  (env-cache-save-to-file)
  (message "Wrote environment variables to %s." env-cache-file))

(defun env-cache-read-from-file ()
  "Read environment variables from file."
  (with-temp-buffer
    (insert-file-contents-literally env-cache-file)
    (read (current-buffer))))

(defun env-cache-maybe-read-from-cache ()
  "Read from `env-cache-file' if it exists."
  (when (file-exists-p env-cache-file)
    (mapc (lambda (pair)
            (let ((name (car pair))
                  (value (cdr pair)))
              (setenv name value)
              (when (string-equal "PATH" name)
                (setq eshell-path-env value
                      exec-path (append (parse-colon-path value) (list exec-directory))))))
          (env-cache-read-from-file))
    ;; Return t to indicate success reading the cache file.
    t))

(unless (env-cache-maybe-read-from-cache)
  (env-cache-refresh))

;; Tell terminal oriented programs not to try to page output.
(setenv "PAGER" "cat")


;;;;; Native Compilation

(custom-set-variables
 ;; Suppress warning popups when the byte compiler issues warnings during async
 ;; native compilation.
 '(native-comp-async-report-warnings-errors nil))


;;;;; Security

(with-eval-after-load 'gnutls
  (defvar gnutls-verify-error t))

(with-eval-after-load 'nsm
  (defvar network-security-level 'high))


;;;;; straight

(custom-set-variables
 '(straight-repository-branch "develop")
 '(straight-check-for-modifications '(check-on-save find-when-checking))
 '(straight-use-package-by-default t)
 '(straight-vc-git-default-clone-depth 1))

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

(autoload #'straight-x-clean-unused-repos "straight-x" nil t)
(autoload #'straight-x-fetch-all "straight-x" nil t)

(defvar straight--repo-cache)
(defun straight-x-delete-package (name)
  "Prompt for package NAME and delete it from the file system."
  (interactive (list (completing-read "Delete package: " (hash-table-keys straight--repo-cache))))
  (let* ((repo (plist-get (gethash name straight--repo-cache) :local-repo))
         (eln-dirs (cl-loop for dir in native-comp-eln-load-path append
                            (directory-files dir t "[^.].*")))
         (modules (mapcar #'file-name-sans-extension
                          (directory-files (straight--repos-dir repo) nil ".*\\.el")))
         (files (cl-loop for dir in eln-dirs append
                         (directory-files dir t (format "%s-[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.eln"
                                                        (regexp-opt modules)))))
         (dirs (list (straight--repos-dir repo) (straight--build-dir repo))))
    (when (yes-or-no-p (format "Delete these files and directories?\n%s\n "
                               (mapconcat #'identity (append dirs files) "\n")))
      (dolist (dir dirs)
        (delete-directory dir 'recursive 'trash)
        (message "Deleted directory %s" dir))
      (dolist (file files)
        (delete-file file 'trash)
        (message "Deleted file %s." file)))))


;;;;; use-package

(custom-set-variables
 '(use-package-always-defer t)
 '(use-package-enable-imenu-support t)
 '(use-package-hook-name-suffix nil))

(straight-use-package 'use-package)

(require 'use-package)

(when init-file-debug
  (setq use-package-verbose t
        use-package-expand-minimally nil
        use-package-compute-statistics t
        debug-on-error t))

(use-package benchmark-init
  :if init-file-debug
  :straight (benchmark-init :host github :repo "kekeimiku/benchmark-init-el")
  :demand t
  :hook
  ;; To disable collection of benchmark data after init is done.
  (emacs-startup-hook . benchmark-init/deactivate))

;; (use-package esup
;;   :straight (esup :host github :repo "phikal/esup")
;;   :config
;;   (setq esup-depth 0))

;;;;; Additional Package Management Configuration

(defvar emacs-start-time)
(defvar straight--repo-cache)

(defun emacs-startup-message ()
  "Display a message after Emacs startup."
  (defconst emacs-load-time
    (float-time (time-subtract (current-time) emacs-start-time)))
  (message "Emacs loaded %d packages in %.1f seconds."
           (length (hash-table-values straight--repo-cache))
           emacs-load-time))

(add-hook 'emacs-startup-hook #'emacs-startup-message)

(defmacro radian-protect-macros (&rest body)
  "Eval BODY, protecting macros from incorrect expansion.
This macro should be used in the following situation: Some form
is being evaluated, and this form contains as a sub-form some
code that will not be evaluated immediately, but will be
evaluated later.  The code uses a macro that is not defined at
the time the top level form is evaluated, but will be defined by
time the sub-form's code is evaluated.  This macro handles its
arguments in some way other than evaluating them directly.  And
finally, one of the arguments of this macro could be interpreted
itself as a macro invocation, and expanding the invocation would
break the evaluation of the outer macro.  You might think this
situation is such an edge case that it would never happen, but
you'd be wrong, unfortunately.  In such a situation, you must
wrap at least the outer macro in this form, but can wrap at any
higher level up to the top level form."
  (declare (indent 0))
  `(eval '(progn ,@body)))

(defun update-emacs-packages ()
  "Synchronously update Emacs packages using `straight'."
  (interactive)
  (straight-pull-package "straight")
  (straight-check-package "straight")
  (straight-pull-all)
  (straight-check-all)
  (url-copy-file
   "https://raw.githubusercontent.com/purcell/exec-path-from-shell/master/exec-path-from-shell.el"
   (expand-file-name "lisp/exec-path-from-shell.el" user-emacs-directory)
   t)
  (message "Finished updating Emacs packages."))

;; TODO This doesn't ever prompt for decisions, like what to do if the repo is
;; dirty.
;; See https://github.com/raxod502/straight.el/issues/103
;; (defun update-emacs-packages-async ()
;;   "Update Emacs packages using `straight'."
;;   (interactive)
;;   (pop-to-buffer
;;    (process-buffer
;;     (make-process
;;      :name "*update-emacs-packages*"
;;      :buffer "*update-emacs-packages*"
;;      :command `("emacs" "--batch"
;;                 "--load" ,(expand-file-name "early-init.el" user-emacs-directory)
;;                 "--load" ,(expand-file-name "init.el" user-emacs-directory)
;;                 "--funcall" "update-emacs-packages-sync")))))

(defun update-system-packages ()
  "Update system packages using the dotfiles update scripts."
  (interactive)
  (async-shell-command "update" "*update-system-packages*"))

(defun update ()
  "Run update scripts for the computer and Emacs."
  (interactive)
  (update-emacs-packages)
  (update-system-packages))


;;;; Third Party Libraries

;; Common libraries that are effectively part of the Emacs core library because
;; they are used by so many packages.

(use-package dash :demand t)

(use-package s :demand t)

(use-package f :demand t)

(use-package async
  :hook
  (dired-mode-hook . dired-async-mode))


;;;; Operating System

;; OS specific configuration

(cl-case system-type
  (darwin
   ;; Mitsuharu Yamamoto's Emacs Mac uses the `mac-*' prefix.
   (setq mac-option-modifier 'meta
         mac-command-modifier 'super)
   (defvar ns-right-alternate-modifier)
   (defvar ns-function-modifier)
   (defvar ns-pop-up-frames)
   ;; Pass right option through to OS.
   (setq ns-right-alternate-modifier 'none
         ns-function-modifier 'hyper
         ns-pop-up-frames nil))
  (windows-nt
   (defvar w32-pass-lwindow-to-system)
   (defvar w32-lwindow-modifier)
   (defvar w32-pass-rwindow-to-system)
   (defvar w32-rwindow-modifier)
   (setq w32-pass-lwindow-to-system nil
         w32-lwindow-modifier 'super
         w32-pass-rwindow-to-system nil
         w32-rwindow-modifier 'super)))


;;;; Bindings

;; Define an additional set of prefixes for commands that don't seem to have a
;; good home elsewhere.

(defmacro bind-prefix (command key description)
  "Define KEY as a prefix COMMAND with DESCRIPTION."
  `(progn
     (define-prefix-command ',command)
     (global-set-key (kbd ,key) ',command)
     (defvar ,command (make-sparse-keymap ,description))))

(bind-prefix m-map "M-m" "A prefix binding")
(bind-prefix m-insert-map "M-m i" "Insert commands")
(bind-prefix m-window-map "M-m w" "Window commands")
(bind-prefix m-file-map "M-m f" "File commands")
(bind-prefix m-help-map "M-m h" "Help commands")
(bind-prefix m-toggle-map "M-m t" "Toggle commands")
(bind-prefix m-search-map "M-m s" "Search commands")
(bind-prefix m-org-map "M-m o" "Org commands")
(bind-prefix m-org-map "M-m M-o" "Org commands")

;; Key bindings to make moving between Emacs and other appliations a bit less
;; jarring. These are mostly based on macOS defaults but an effor has been made
;; to work on Windows and Linux. That is why there are multiple bindings for
;; many commands. They can be overridden by the OS specific configurations
;; below.

(bind-keys
 ;; Basic macOS keys
 ;; ("s-z" . undo)
 ;; ("s-x" . kill-region)
 ;; ("s-c" . kill-ring-save)
 ;; Editing
 ("C-d" . delete-forward-char)
 ("M-c" . capitalize-dwim)
 ("M-l" . downcase-dwim)
 ("M-u" . upcase-dwim)
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
 ("C-s-s" . isearch-forward-symbol-at-point)
 ("M-o" . other-window)
 ("s-b" . switch-to-buffer)
 ("s-S-b" . switch-to-buffer-other-window)
 ("s-\`" . other-frame)
 ("C-\`" . other-frame)
 ("s-w" . delete-window)
 ("s-W" . delete-other-windows)
 ("C-s-w" . delete-frame)
 ("s-h" . ns-do-hide-emacs)
 ("s-H" . ns-do-hide-others)
 ("C-c U" . revert-buffer)
 ("s-." . repeat))


;;;; Persistence

;; Persist Emacs session data.

;; Store all backup and autosave files in their own directory since it is bad to
;; clutter project directories. This also backs up TRAMP files locally.
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
      ;; Don't create `#file-name' lockfiles in $PWD. Lockfiles are useful but
      ;; they generate too much activity from tools watching for changes during
      ;; development.
      create-lockfiles nil
      ;; Increase undo limit to 5MB per buffer.
      undo-limit 5242880)

(use-package saveplace
  :config
  (save-place-mode)
  :hook
  (find-file-hook . save-place-find-file-hook))

(use-package recentf
  :defer 7
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'never)

  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))

  :config
  (recentf-mode)

  :hook
  (dired-mode-hook . recentf-add-dired-directory))

(use-package autorevert
  :custom
  (auto-revert-verbose nil)
  :hook
  (after-change-major-mode-hook . auto-revert-mode))

(use-package savehist
  :demand t
  :custom
  (savehist-autosave-interval 60)
  (history-delete-duplicates t)
  (savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring
                                   file-name-history
                                   read-expression-history
                                   command-history
                                   extended-command-history
                                   window-config-alist
                                   magit-read-rev-history
                                   fiat-theme))
  :config
  (put 'kill-ring 'history-length 300)
  (savehist-mode))


;;;; Private

;; If it exists, load the private configuration file.
(load "~/private/private.el" t t nil t)


;;;; Appearance

;; Set up visual UI and theme stuff.

(setq enable-recursive-minibuffers t)

(custom-set-variables '(inhibit-splash-screen t))

;; Beeping is REALLY NOT OK
(setq visible-bell t
      ;; Show keystrokes right away.
      echo-keystrokes 0.01)

;; GUI Configuration
(when window-system
  (setq
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
    ;; FIXME "Input Mono-14" seems to have some spacing issues.
    (some-font '("Input Mono-14" "Monaco-13" "Lucida Console-12"
                 "DejaVu Sans Mono-12" "Inconsolata-14"))
    "The default font to use for fixed pitch applications.")

  (defvar m-variable-pitch-font
    (some-font '("Avenir-17" "Calibri-15" "Helvetica Neue-15" "Helvetica-15"
                 "Georgia-15"))
    "The default font to use for variable pitch applications.")

  (defvar m-fallback-font
    (some-font '("Arial Unicode MS-12" "DejaVu Sans Mono-12"))
    "The fallback font for unicode glyphs the other fonts don't support.")

  ;; (dolist (fontset '("fontset-default" "fontset-standard" "fontset-startup"))
  ;;   (set-fontset-font fontset '(#x000000 . #x3FFFFF) m-fallback-font)
  ;;   (set-fontset-font fontset nil m-fallback-font))

  (set-face-font 'default m-fixed-pitch-font)
  (set-face-font 'fixed-pitch m-fixed-pitch-font)

  (set-face-font 'variable-pitch m-variable-pitch-font)

  (with-eval-after-load 'mwheel
    (setq mouse-wheel-follow-mouse t
          mouse-wheel-scroll-amount '(1 ((shift) . 1)))))

(use-package mixed-pitch
  :if window-system
  :custom
  (mixed-pitch-set-height t)
  :config

  (defvar mixed-pitch-exclude-modes
    '(dns-mode yaml-mode nxml-mode)
    "Modes excluded from `mixed-pitch-mode'.")

  (defun maybe-enable-mixed-pitch-mode ()
    "Maybe enable `mixed-pitch-mode'."
    (unless (seq-some (lambda (mode) (derived-mode-p mode))
                      mixed-pitch-exclude-modes)
      (mixed-pitch-mode)))

  :hook
  (text-mode-hook . maybe-enable-mixed-pitch-mode)
  (Info-mode-hook . mixed-pitch-mode))

(use-package fontify-face
  :if window-system
  :hook
  (emacs-lisp-mode-hook . fontify-face-mode))

;; Wrap text at the end of a line like a word processor.
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

(defun get-attribute (object propname attribute name)
  "Get the value of NAME from OBJECT.

PROPNAME, ATTRIBUTE, and NAME are symbols which drill down to
access the individual Alist element we are after.

See `get' and `theme-attribute'."
  (let ((name (if (stringp name) (intern name) name)))
    (cl-some (lambda (e) (when (and (eq attribute (car e)) (eq name (cadr e)))
                           (car (cdr (cdr (cdr e))))))
             (get (car object) propname))))

(defun theme-attribute (attribute name)
  "Get the ATTRIBUTE identified by NAME from the current theme settings.)

Example usage

  (theme-attribute 'default :background)

Note that unlike `face-attribute', which gets the *current*
attribute value as displayed on the screen, this function gets
the attribute as specified in the *original* theme settings.
This is useful when you switch themes and want to calculate new
faces derived from existing ones."
  (get-attribute custom-enabled-themes 'theme-settings attribute name))

(defun theme-face (face)
  "Get the FACE from the current theme.

See `theme-attribute'."
  (theme-attribute 'theme-face face))

(defun theme-face-attribute (face attribute)
  "Get the ATTRIBUTE of the FACE from the current theme.

See `theme-attribute'."
  (plist-get (face-spec-choose (theme-face face)) attribute))

(defvar fiat-theme 'whiteboard
  "The current theme.")

(defvar fiat-themes '((light . modus-operandi)
                      (dark . modus-vivendi))
  "The light and dark themes.")

(defun fiat-save-theme (theme &rest _)
  "Save the current THEME."
  (setq fiat-theme theme))

(advice-add #'load-theme :after #'fiat-save-theme)

(defun fiat-current ()
  "Get the Alist key corresponding to the current theme."
  (cl-some (lambda (theme) (when (member (alist-get theme fiat-themes)
                                         custom-enabled-themes)
                             theme))
           (mapcar #'car fiat-themes)))

(defun fiat (&optional key)
  "Let the Emacs theme be changed.

KEY is a key in the Alist `fiat-themes'.  If KEY isn't specified
then choose the next key in the Alist `fiat-themes'."
  (interactive)
  (unless key
    (setq key
          (car (elt fiat-themes
                    (mod (1+ (cl-position (or (fiat-current)
                                              (caar fiat-themes))
                                          (mapcar #'car fiat-themes)))
                         (length fiat-themes))))))
  (unless (member (alist-get key fiat-themes) custom-enabled-themes)
    (load-theme (alist-get key fiat-themes) t)))

(defun fiat-lux ()
  "Let the Emacs theme be light."
  (interactive)
  (fiat 'light))

(defun fiat-nox ()
  "Let the Emacs theme be dark."
  (interactive)
  (fiat 'dark))

;; The patch from d12frosted/emacs-plus causes this function to be called when
;; the macOS theme is changed.
(add-hook 'ns-system-appearance-change-functions #'fiat)

(bind-keys
 ("C-M-s-t" . fiat)
 ("M-m t t" . fiat))

(use-package window-highlight
  :if (and window-system
           (>= emacs-major-version 27)
           (not (eq window-system 'windows-nt)))
  :demand t
  :straight (window-highlight :host github :repo "dcolascione/emacs-window-highlight")
  :config
  ;; Sometimes on startup, Emacs doesn't realize it's in focus? I think this is
  ;; because of the way macOS starts Emacs (because starting it from the command
  ;; line doesn't exhibit this behavior). Anyway, it doesn't seem too terrible
  ;; to go ahead and set it manually.
  (set-frame-parameter (selected-frame) 'last-focus-update t)
  (window-highlight-mode))

;; (use-package doom-themes
;;   :demand t
;;   :config
;;   (doom-themes-visual-bell-config))

(defface visual-bell '((t (:background "#FF3366" :foreground "#FFFFFF")))
  "Face to use for the mode-line when `doom-themes-visual-bell-config' is used."
  :group 'doom-themes)

(defun visual-bell-fn ()
  "Blink the mode-line briefly. Set `ring-bell-function' to this to use it.
This is a modified version of `doom-themes-ext-visual-bell-fn'."
  (let ((doom-themes--bell-cookie (face-remap-add-relative 'mode-line 'visual-bell)))
    (force-mode-line-update)
    (run-with-timer 0.15 nil
                    (lambda (cookie buf)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (face-remap-remove-relative cookie)
                          (force-mode-line-update))))
                    doom-themes--bell-cookie
                    (current-buffer))))

(setq ring-bell-function #'visual-bell-fn)

(use-package modus-themes
  :demand t
  :custom
  (modus-themes-slanted-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-intense-paren-match 'intense))

(defun color-blend (color1 color2 alpha)
  "Blends COLOR1 onto COLOR2 with ALPHA.
COLOR1 and COLOR2 should be color names (e.g. \"white\") or RGB
triplet strings (e.g. \"#ff12ec\").
Alpha should be a float between 0 and 1.

Stolen from solarized."
  (apply #'color-rgb-to-hex
         (-zip-with (lambda (it other)
                      (+ (* alpha it) (* other (- 1 alpha))))
                    (color-name-to-rgb color1)
                    (color-name-to-rgb color2))))

(defun shorten-file-name (file-name &optional max-length)
  "Shorten FILE-NAME to no more than MAX-LENGTH characters."
  (let* ((max-length (or max-length 40))
         (ellipsis (if (char-displayable-p ?…) "…" "..."))
         (length (length file-name)))
    (if (<= length max-length)
        file-name
      (concat ellipsis
              (substring file-name (- length max-length (length ellipsis)) length)))))

;; TODO This version of `shorten-file-name' causes a stack overflow when
;; multi-stage TRAMP paths are visited.  Fix that and use it instead.
;; (defun shorten-file-name (file-name &optional max-length)
;;   "Shorten FILE-NAME to no more than MAX-LENGTH characters."
;;   (let* ((max-length (or max-length 40))
;;          (separator (if (eq system-type 'windows-nt) "\\" "/"))
;;          (ellipsis (concat (if (char-displayable-p ?…) "…" "...") separator))
;;          (right (split-string (abbreviate-file-name file-name) separator))
;;          left output)
;;     (while (and (< 1 (length right))
;;                 (< max-length (length (string-join (append left right) separator))))
;;       (push (let ((r (pop right)))
;;               (if (< 0 (length r))
;;                   (substring r 0 1)
;;                 ""))
;;             left))
;;     (setq output (string-join (append (reverse left) right) separator))
;;     (if (< max-length (length output))
;;         (concat ellipsis
;;                 (substring output (- (length output) (length ellipsis) max-length)))
;;       output)))

(use-package mood-line
  :demand t
  :config
  (defvar mood-line-selected-window (frame-selected-window)
    "Selected window.")

  (defun mood-line--set-selected-window ()
    "Set the variable `fiat-selected-window' appropriately.
This is used to determine whether the current window is active."
    (unless (minibuffer-window-active-p (frame-selected-window))
      (setq mood-line-selected-window (frame-selected-window))
      (force-mode-line-update)))

  ;; Executes after a window (not a buffer) has been created, deleted, or moved.
  (add-hook 'window-configuration-change-hook #'mood-line--set-selected-window)

  ;; Executes after the `buffer-list' changes.
  (add-hook 'buffer-list-update-hook #'mood-line--set-selected-window)

  (defun mood-line-window-active-p ()
    "Return whether the current window is active."
    (eq mood-line-selected-window (selected-window)))

  (defun mood-line-segment-hostname ()
    "Return the remote hostname for the current buffer.
Return nil if the buffer is local."
    (when (file-remote-p default-directory)
      (let* ((dissected (tramp-dissect-file-name default-directory)))
        (concat
         (when-let* ((user (tramp-file-name-user dissected))
                     (face (if (string= user "root") 'error 'warning)))
           (propertize (concat " " user " ") 'face face))
         (propertize (concat " " (tramp-file-name-host dissected) " ")
                     'face 'highlight)))))

  (defun mood-line--make-xpm (face width height)
    "Create an XPM bitmap via FACE, WIDTH and HEIGHT.

Inspired by `powerline''s `pl/make-xpm'."
    (when (and (display-graphic-p)
               (image-type-available-p 'xpm))
      (propertize
       " " 'display
       (let ((data (make-list height (make-list width 1)))
             (color (or (face-background face nil t) "None")))
         (ignore-errors
           (create-image
            (concat
             (format
              "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
              (length (car data)) (length data) color color)
             (apply #'concat
                    (cl-loop with idx = 0
                             with len = (length data)
                             for dl in data
                             do (cl-incf idx)
                             collect
                             (concat
                              "\""
                              (cl-loop for d in dl
                                       if (= d 0) collect (string-to-char " ")
                                       else collect (string-to-char "."))
                              (if (eq idx len) "\"};" "\",\n")))))
            'xpm t :ascent 'center))))))

  (defvar mood-line-height 22
    "The height of the mode-line in pixels.")

  (defun mood-line--refresh-bar ()
    "Refresh the bar."
    (setq mood-line-bar (mood-line--make-xpm 'mode-line 1 mood-line-height)))

  (defvar mood-line-bar (mood-line--refresh-bar)
    "A bar to increase the height of the mode-line.

Inspired by `doom-modeline'.")

  (defun mood-line-segment-bar ()
    "Display a bar."
    mood-line-bar)

  (defvar-local mood-line-buffer-name nil
    "The buffer name as displayed in `mood-line'.")

  (defun mood-line--refresh-buffer-name (&rest _)
    "Refresh the buffer name."
    (setq-local mood-line-buffer-name
                (propertize
                 (concat " " (shorten-file-name (format-mode-line "%b")) " ")
                 'face 'mode-line-buffer-id)))

  (add-hook 'buffer-list-update-hook #'mood-line--refresh-buffer-name)
  (add-hook 'window-configuration-change-hook #'mood-line--refresh-buffer-name)
  (add-hook 'after-set-visited-file-name-hook #'mood-line--refresh-buffer-name)

  (defun mood-line-segment-buffer-name ()
    "Displays the name of the current buffer in the mode-line."
    mood-line-buffer-name)

  (defun mood-line-segment-modified ()
    "Displays a color-coded buffer modification/read-only indicator in the mode-line."
    (when (and buffer-file-name
               (not (string-match-p "\\*.*\\*" (buffer-name)))
               (buffer-modified-p))
      "● "))

  (defvar flycheck-current-errors)

  (defun mood-line--update-flycheck-segment (&optional status)
    "Update `mood-line--flycheck-text' against the reported flycheck STATUS."
    (setq mood-line--flycheck-text
          (pcase status
            ('finished (if flycheck-current-errors
                           (let-alist (flycheck-count-errors flycheck-current-errors)
                             (let ((sum (+ (or .error 0) (or .warning 0))))
                               (propertize (concat ;"⚑"
                                            (number-to-string sum)
                                            " ")
                                           'face (if .error
                                                     'mood-line-status-error
                                                   'mood-line-status-warning))))
                         (propertize "✔ " 'face 'mood-line-status-success)))
            ('running (propertize "⧖ " 'face 'mood-line-status-info))
            ('errored (propertize "✖ " 'face 'mood-line-status-error))
            ('interrupted (propertize "❙❙ " 'face 'mood-line-status-neutral))
            ('no-checker ""))))

  (defun mood-line-segment-flycheck ()
    "Display color-coded flycheck information in the mode-line (if available)."
    (when (mood-line-window-active-p) mood-line--flycheck-text))

  (defun mood-line-segment-misc-info ()
    "Display the current value of `mode-line-misc-info' in the mode-line."
    (when (mood-line-window-active-p)
      (apply #'concat
             (mapcar (lambda (e)
                       (let ((s (format-mode-line (cadr e))))
                         (if (string-blank-p s)
                             ""
                           (concat (string-trim s) " "))))
                     mode-line-misc-info))))

  (defun mood-line-segment-projectile ()
    "Display the projectile project name."
    (when (and (not (file-remote-p default-directory))
               (mood-line-window-active-p)
               (fboundp #'projectile-project-name))
      (propertize (concat " " (projectile-project-name) " ")
                  'face 'font-lock-variable-name-face)))

  (defun mood-line-pyvenv-info ()
    "When inside a Python venv project, display its name."
    (when (boundp 'pyvenv-virtual-env)
      (setf (alist-get 'pyvenv mode-line-misc-info)
            (list (when pyvenv-virtual-env
                    (concat "pyvenv:" pyvenv-virtual-env-name))))))

  (add-hook 'post-command-hook #'mood-line-pyvenv-info)

  (defun outline-minor-mode-info ()
    "Display an indicator when `outline-minor-mode' is enabled."
    (setf (alist-get 'outline-minor-mode mode-line-misc-info)
          (list (when (bound-and-true-p outline-minor-mode) "Ⓞ"))))

  (add-hook 'outline-minor-mode-hook #'outline-minor-mode-info)

  (defun edebug-mode-info (_symbol newval _operation _where)
    "Display an indicator when `edebug' is active.

Watches `edebug-active' and sets the mode-line when it changes."
    (setf (alist-get 'edebug-mode mode-line-misc-info)
          (list (when newval "ED"))))

  (add-variable-watcher 'edebug-active #'edebug-mode-info)

  (defvar buffer-narrowed nil
    "Non-nil if the buffer is currently narrowed.")

  (defun narrowed-info (&optional _start _end)
    "Display an indicator when the buffer is narrowed."
    (setf (alist-get 'buffer-narrowed mode-line-misc-info)
          (when (setq buffer-narrowed (buffer-narrowed-p))
            (list "n"))))

  ;; npostavs suggests hooking `post-command-hook'.
  ;; https://emacs.stackexchange.com/questions/33288
  (add-hook 'post-command-hook #'narrowed-info)

  (defvar parinfer--mode)
  (defvar parinfer-lighters)

  (defun parinfer-mode-info (&optional _mode)
    "Display an indicator when `parinfer-mode' is enabled."
    (setf (alist-get 'parinfer-mode mode-line-misc-info)
          (when (bound-and-true-p parinfer-mode)
            (list (if (eq 'paren parinfer--mode)
                      (cdr parinfer-lighters)
                    (car parinfer-lighters))))))

  (add-hook 'parinfer-mode-enable-hook #'parinfer-mode-info)
  (add-hook 'parinfer-mode-disable-hook #'parinfer-mode-info)
  (add-hook 'parinfer-switch-mode-hook #'parinfer-mode-info)
  ;; KLUDGE None of the above hooks get called when parinfer is initialized
  ;; so we have to catch up somehow. I'm sure there's a better way.
  (add-hook 'window-state-change-hook #'parinfer-mode-info)

  ;; (defvar parinfer-rust-lighters
  ;;   '(("smart" . "s")
  ;;     ("paren" . ")")
  ;;     ("indent" . "➠"))
  ;;   "Mode line indication for `parinfer-rust-mode'.")

  ;; (defvar parinfer-rust-enabled)

  ;; (defun parinfer-rust-mode-info (&optional _mode)
  ;;   "Display an indicator when `parinfer-rust-mode' is enabled."
  ;;   (setf (alist-get 'parinfer-rust-mode mode-line-misc-info)
  ;;         (when parinfer-rust-enabled
  ;;           (list (assoc-default "smart" parinfer-rust-lighters)))))

  ;; (add-hook 'window-state-change-hook #'parinfer-rust-mode-info)

  (defun hs-minor-mode-info ()
    "Display an indicator when `hs-minor-mode' is enabled."
    (setf (alist-get 'hs-minor-mode mode-line-misc-info)
          (list (when (bound-and-true-p hs-minor-mode) '"hs"))))

  (add-hook 'hs-minor-mode-hook #'hs-minor-mode-info)

  (defun mood-line-segment-major-mode ()
    "Displays the current major mode in the mode-line."
    (when (mood-line-window-active-p)
      (propertize (concat " " (format-mode-line mode-name) " ")
                  'face (if (mood-line-window-active-p)
                            'mode-line-emphasis
                          'mode-line))))

  ;; (defun mood-line--format (left right)
  ;;   "Return a string of `window-width' length containing LEFT and RIGHT, aligned respectively."
  ;;   (concat
  ;;    left
  ;;    " "
  ;;    (propertize " " 'display `((space :align-to (- right ,(1- (length right))))))
  ;;    right))

  (mood-line-mode)

  (setq-default mode-line-format
                '((:eval
                   (mood-line--format
                    ;; Left
                    (format-mode-line
                     '((:eval (mood-line-segment-bar))
                       (:eval (mood-line-segment-hostname))
                       (:eval (mood-line-segment-buffer-name))
                       (:eval (mood-line-segment-modified))
                       (:eval (mood-line-segment-major-mode))
                       (:eval (mood-line-segment-anzu))
                       (:eval (mood-line-segment-multiple-cursors))))
                    ;; Right
                    (format-mode-line
                     '((:eval (mood-line-segment-process))
                       (:eval (mood-line-segment-projectile))
                       (:eval (mood-line-segment-flycheck))
                       (:eval (mood-line-segment-misc-info)))))))))

(defun theme-reset (&rest _)
  "Remove all current themes before loading a new theme."
  (mapc #'disable-theme custom-enabled-themes))

(defun theme-customize-faces (&rest _)
  "Customize faces after a theme is loaded.

This sets things up for `window-highlight' and `mode-line'."
  (let* ((active-bg (or (theme-face-attribute 'default :background)
                        (if (eq (frame-parameter nil 'background-mode) 'light)
                            "#FFF" "#000")))
         (inactive-bg (color-blend active-bg
                                   (theme-face-attribute 'default :foreground)
                                   0.95)))
    ;; cursor
    (set-face-background 'cursor "magenta")
    ;; mode-line
    (set-face-attribute 'mode-line nil :box nil)
    (set-face-attribute 'mode-line-inactive nil :box nil)
    (with-eval-after-load 'window-highlight
      (set-face-background 'default inactive-bg)
      (set-face-background 'fringe  inactive-bg)
      (set-face-foreground 'vertical-border inactive-bg)
      (set-face-background 'window-highlight-focused-window active-bg))
    (with-eval-after-load 'org
      (set-face-attribute 'org-document-title nil :height 1.5)
      (set-face-attribute 'org-level-1 nil :height 1.4)
      (set-face-attribute 'org-level-2 nil :height 1.2)
      (set-face-attribute 'org-level-3 nil :height 1.1))
    (with-eval-after-load 'outline
      (set-face-attribute 'outline-1 nil :height 1.4)
      (set-face-attribute 'outline-2 nil :height 1.2)
      (set-face-attribute 'outline-3 nil :height 1.1))
    (with-eval-after-load 'eldoc-box
      (set-face-background
       'eldoc-box-body
       (color-blend active-bg (theme-face-attribute 'default :foreground) 0.8))))
  (with-eval-after-load 'mood-line
    (mood-line--refresh-bar)))

(advice-add #'load-theme :before #'theme-reset)
(advice-add #'load-theme :after #'theme-customize-faces)

(when (display-graphic-p)
  (load-theme fiat-theme t))

(use-package hl-line
  :defer 2
  :init
  (defvar hl-line-disabled-mode-list nil
    "Major modes in which to not enable `hl-line-mode'.")

  :custom
  (global-hl-line-sticky-flag t)

  :config
  (defun maybe-enable-hl-line-mode ()
    (unless (cl-some #'derived-mode-p hl-line-disabled-mode-list)
      (hl-line-mode +1)))

  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (maybe-enable-hl-line-mode)))

  :hook
  (after-change-major-mode-hook . maybe-enable-hl-line-mode))

(use-package page-break-lines
  :hook
  (emacs-lisp-mode-hook . page-break-lines-mode))

(use-package darkroom
  :bind
  (:map m-toggle-map
        ("d" . darkroom-mode)))

(use-package hl-todo
  :custom
  (hl-todo-keyword-faces
   '(("TODO" . "magenta")
     ("FIXME" . "magenta")
     ("\\?\\?\\?+" . "magenta")
     ("WIP" . "lime green")
     ("WORK" . "lime green")
     ("NEXT" . "lime green")
     ("NOTE" . "purple")
     ("WAIT" . "orange")
     ("KLUDGE" . "orange")
     ("HACK" . "orange")
     ("TEMP" . "orange")
     ("XXX+" . "orange")
     ("DONE" . "gray")))

  :hook
  (prog-mode-hook . hl-todo-mode)
  :bind
  (:map hl-todo-mode-map
        ("M-s h i" . hl-todo-insert)
        ("M-s h C-p" . hl-todo-previous)
        ("M-s h C-n" . hl-todo-next)
        ("M-s h o" . hl-todo-occur)))

;; (use-package font-lock-studio
;;   :commands
;;   font-lock-studio)


;;;; Navigation

;; Navigation tools

(use-package outline
  :hook
  (prog-mode-hook . outline-minor-mode)
  :bind
  (:map outline-minor-mode-map
        ("C-<tab>" . outline-cycle)
        ("M-p" . outline-previous-heading)
        ("M-n" . outline-next-heading)))

(use-package outline-minor-faces
  :after outline
  :config

  ;; TODO
  ;;   (defun outline-minor-faces--comment-matcher (regexp)
  ;;     "Return a matcher that matches REGEXP only in comments.
  ;; Intended to replace `outline-minor-faces--syntactic-matcher'."
  ;;     (if font-lock-keywords-only
  ;;         regexp
  ;;       (lambda (limit)
  ;;         (and (re-search-forward regexp limit t)
  ;;              ;; this apparently doesn't work.
  ;;              (nth 4 (syntax-ppss (match-end 0)))))))

  ;; Another idea that doesn't work. See https://emacs.stackexchange.com/questions/14269/how-to-detect-if-the-point-is-within-a-comment-area
  ;; (let* ((fontfaces (get-text-property pos 'face)))
  ;;   (when (not (listp fontfaces))
  ;;     (setf fontfaces (list fontfaces)))
  ;;   (or (member 'font-lock-comment-face fontfaces)
  ;;       (member 'font-lock-comment-delimiter-face fontfaces))))

  ;; (advice-add #'outline-minor-faces--syntactic-matcher
  ;;             :override #'outline-minor-faces--comment-matcher)

  ;; Only comment headings should be part of the outline, not top level forms
  ;; or autoloads. See `lisp-mode' for the default.
  ;; (setq-local outline-minor-faces-regexp ";;;\\(;* [^ \t\n]\\)")
  :hook
  (emacs-lisp-mode-hook . outline-minor-faces-add-font-lock-keywords))

(use-package outorg
  :after outline
  :bind
  (:map outline-minor-mode-map
        ("M-# #" . outorg-edit-as-org)))

(defun fullscreen ()
  "Toggle fullscreen mode."
  (interactive)
  (set-frame-parameter
   nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun scroll-window-up ()
  "Scroll the buffer up, keeping point in place relative to the window."
  (interactive)
  (let ((p (point)))
    (scroll-down-command 4)
    (goto-char p)))

(defun scroll-window-down ()
  "Scroll the buffer up, keeping point in place relative to the window."
  (interactive)
  (let ((p (point)))
    (scroll-up-command 4)
    (goto-char p)))

(defun scroll-up-margin ()
  "Move point to the top of the window.

If it's already there, scroll `scroll-margin' lines up."
  (interactive)
  (let ((line (line-number-at-pos))
        (line-beg (line-number-at-pos (window-start))))
    (if (<= (- line line-beg) scroll-margin)
        (forward-line (- scroll-margin))
      (forward-line (+ (- line-beg line) scroll-margin)))))

(defun scroll-down-margin ()
  "Move point to the bottom of the window.

If it's already there, scroll `scroll-margin' lines down."
  (interactive)
  (let ((line (line-number-at-pos))
        ;; 2 is for minibuffer + modeline.
        (line-end (- (line-number-at-pos (window-end)) 2)))
    (if (<= (- line-end line) scroll-margin)
        (forward-line scroll-margin)
      (forward-line (- line-end line scroll-margin)))))

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
that.  So, we find only ones which are associated with a magic
string or file extension."
  (delete-dups (mapcar #'cdr (append magic-mode-alist
                                     auto-mode-alist
                                     magic-fallback-mode-alist))))

(defun scratch-buffer (arg)
  "Create or go to a scratch buffer.

If ARG is provided then create a new buffer regardless of whether
one exists already."
  (interactive "P")
  (let* ((default-directory code-directory)
         (uniquify-buffer-name-style nil)
         (persp-name (persp-name (persp-curr)))
         (name (if (string= "main" persp-name)
                   "*scratch*"
                 (format "*scratch* (%s)" persp-name)))
         (buffer (if arg
                     (generate-new-buffer name)
                   (get-buffer-create name)))
         (win (get-buffer-window buffer)))
    (if win
        (select-window win)
      (switch-to-buffer buffer))))

(defun scratch-buffer-other-window (arg)
  "Create or go to a scratch buffer in the current mode.

If ARG is provided then create a new buffer regardless of whether
one exists already.

See `scratch-buffer'."
  (interactive "P")
  (switch-to-buffer-other-window (current-buffer))
  (scratch-buffer arg))

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
  (when (= (count-windows) 2)
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

(use-package ffap
  :preface
  (defun find-file-at-point-with-line (&optional filename)
    "Open FILENAME at point and move point to line specified next to file name."
    (interactive)
    (require 'ffap)
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
  :bind
  ("C-x M-f" . find-file-at-point)
  ("C-c C-f" . find-file-at-point-with-line))

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
notation.  When the file name has line numbers and optionally
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
  (prog-mode-hook . goto-address-prog-mode)
  (text-mode-hook . goto-address-mode))

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

  (defun bug-reference-dispatch-url-github-or-gitlab (_type ref)
    "With Bug TYPE and REF, return a complete URL.

Idea stolen from https://github.com/arnested/bug-reference-github."
    (require 'vc-git)
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
  :defer 10

  :preface
  (defun winner-wrong-window ()
    "Open the last opened buffer in the other window."
    (interactive)
    (let* ((current (window-list))
           (previous (save-window-excursion (winner-undo) (window-list)))
           (window (seq-some (lambda (w) (not (memq w previous))) current))
           (buffer (window-buffer window)))
      (winner-undo)
      (switch-to-buffer-other-window buffer)))

  :config
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

(use-package perspective
  :preface
  (defun persp-names-reverse ()
    "Like `persp-names' with 'created but latest is last."
    (mapcar #'persp-name
            (sort (hash-table-values (perspectives-hash))
                  (lambda (a b)
                    (time-less-p (persp-created-time a)
                                 (persp-created-time b))))))

  (advice-add #'persp-names :override #'persp-names-reverse)

  (defun persp-set-frame-title ()
    "Set the frame title with the current perspective name."
    (setq frame-title-format (mapconcat (lambda (name)
                                          (if (string= name (persp-current-name))
                                              (concat "[ " name " ]")
                                            (concat "  " name "  ")))
                                        (persp-names)
                                        "     ")))

  (defun choose-by-number (options &optional prompt)
    "Display a list and choose among OPTIONS by pressing its number."
    (interactive)
    (string-to-number
     (char-to-string
      (read-char
       (string-join
        (cons (or prompt "Choose by pressing a number:")
              (seq-map-indexed (lambda (p n) (format "%d: %s" (1+ n) p)) options))
        "\n")))))

  (defun persp-switch-nth (n)
    "Switch to the N-th perspective."
    (interactive (list (choose-by-number (persp-names))))
    (persp-switch (nth (1- n) (persp-names))))

  ;; Create `persp-switch-to-X' and key bindings.
  (dolist (n (number-sequence 1 9))
    (let ((f (intern (format "persp-switch-to-%d" n))))
      (bind-key (format "C-x x %d" n) f)
      (bind-key (format "H-%d" n) f)
      (eval `(defun ,f ()
               ,(format "Switch to perspective number %d." n)
               (interactive)
               (persp-switch-nth ,n)))))

  (defun persp-switch-to-org ()
    "Switch to the Org perspective."
    (interactive)
    (let ((initialized (member "org" (persp-names))))
      (persp-switch "org")
      (unless initialized
        (find-file (expand-file-name "TODO.org" org-directory)))))

  (defun persp-switch-to-main ()
    "Switch to the Main perspective."
    (interactive)
    (persp-switch "main"))

  :custom
  ;; Don't show modestring on the modeline; it's displayed on the frame title
  ;; instead.
  (persp-show-modestring nil)
  (persp-sort 'created)
  (persp-state-default-file (expand-file-name "var/perspective" user-emacs-directory))
  (persp-modestring-dividers `("" "" ,(propertize "|" 'face 'shadow)))

  :config
  (make-directory (file-name-directory persp-state-default-file) t)

  :hook
  (emacs-startup-hook . persp-mode)
  (persp-activated-hook . persp-set-frame-title)
  (kill-emacs-hook . persp-state-save)

  :bind
  (:map persp-mode-map
        ("C-x x l" . persp-state-load)
        ("C-x x C-s" . persp-state-save)
        ("C-x x x" . persp-switch-nth)
        ("C-s-[" . persp-prev)
        ("H-[" . persp-prev)
        ("H-]" . persp-next)
        ("C-s-]" . persp-next)
        ("s-o" . persp-switch-to-org)
        ("s-m" . persp-switch-to-main)
        ("s-;" . persp-switch)
        ("M-s-p" . persp-switch)))

;; Create friendly names for buffers with the same name
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; scratch
(setq initial-scratch-message nil
      initial-major-mode 'indented-text-mode)

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

;; hs-minor-mode for folding top level forms
(use-package hideshow
  :custom
  (hs-hide-comments-when-hiding-all nil)
  :bind
  ("C-c C-h" . hs-minor-mode)
  (:map hs-minor-mode-map
        ("C-<tab>" . hs-toggle-hiding)))

(use-package symbol-overlay
  :hook
  ((prog-mode-hook helpful-mode-hook help-mode-hook) . symbol-overlay-mode)
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
  ((css-mode-hook emacs-lisp-mode-hook js-mode-hook sass-mode-hook)
   . rainbow-mode))

(use-package crux
  :config
  (with-eval-after-load 'org
    (bind-key "C-a" #'crux-move-beginning-of-line org-mode-map))

  :bind
  ([remap move-beginning-of-line] . crux-move-beginning-of-line)
  ("M-s-<backspace>" . crux-kill-line-backwards)
  ("C-s-c" . crux-duplicate-current-line-or-region)
  (:map m-window-map
        ("k" . crux-kill-other-buffers))
  (:map m-file-map
        ("d" . crux-delete-file-and-buffer)
        ("r" . crux-rename-file-and-buffer))
  (:map emacs-lisp-mode-map
        ("C-x C-j" . crux-eval-and-replace))
  (:map lisp-interaction-mode-map
        ("C-x C-j" . crux-eval-and-replace)))

(defmacro specialize-beginning-of-buffer (mode &rest forms)
  "Define a special version of `beginning-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-min' and then FORMS are evaluated.  If the point did
not change because of the evaluation of FORMS, jump
unconditionally to `point-min'.  This way repeated invocations
toggle between real beginning and logical beginning of the
buffer.

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

(defvar dired-mode-map)
(specialize-beginning-of-buffer dired
  (while (not (ignore-errors (dired-get-filename))) (dired-next-line 1)))
(specialize-end-of-buffer dired (dired-previous-line 1))

(specialize-beginning-of-buffer occur (occur-next 1))
(specialize-end-of-buffer occur (occur-prev 1))

(defvar ibuffer-mode-map)
(specialize-beginning-of-buffer ibuffer (ibuffer-forward-line 1))
(specialize-end-of-buffer ibuffer (ibuffer-backward-line 1))

(defvar vc-dir-mode-map)
(specialize-beginning-of-buffer vc-dir (vc-dir-next-line 1))
(specialize-end-of-buffer vc-dir (vc-dir-previous-line 1))

(defvar recentf-dialog-mode-map)
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

(use-package ibuffer
  :commands
  ibuffer-forward-line
  ibuffer-backward-line

  :bind
  ("C-x M-b" . ibuffer))

(use-package bufler
  :bind
  ("C-x C-b" . bufler-list))

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

;; TODO Use this I guess?
(defmacro window-config-make (name &rest exprs)
  "Make window-config NAME command, running EXPRS.

Each EXPR should create one window."
  (declare (indent defun))
  `(defun ,(intern name) (arg)
     ,(format "Set up a window configuration for %s." name)
     (interactive "P")
     (when arg (message "TODO: Create new perpsective and open it there."))
     (delete-other-windows)
     ,(car exprs)
     (let ((first-window (selected-window)))
       (switch-to-buffer-other-window (current-buffer))
       ,(cadr exprs)
       (select-window first-window))))

(defun window-config-dotemacs ()
  "Set up dotemacs window config."
  (interactive)
  (delete-other-windows)
  (find-file "~/.emacs.d/init.el")
  (switch-to-buffer-other-window (current-buffer))
  (find-file "~/.emacs.d/TODO.org")
  (other-window 1))

(bind-keys
 ("s-n" . scratch-buffer)
 ("s-N" . scratch-buffer-other-window)
 ("C-c C-n" . scratch-buffer)
 ("C-c M-n" . scratch-new-buffer-other-window)
 ("C-S-p" . scroll-up-margin)
 ("C-S-n" . scroll-down-margin)
 ("H-p" . scroll-window-up)
 ("H-n" . scroll-window-down)

 ;; Full screen
 ("C-s-f" . fullscreen)

 ;; Quick switch buffers
 ("s-}" . next-buffer)
 ("C-c }" . next-buffer)
 ("s-{" . previous-buffer)
 ("C-c {" . previous-buffer)

 ;; windmove
 ("H-a" . windmove-left)
 ("H-d" . windmove-right)
 ("H-w" . windmove-up)
 ("H-s" . windmove-down)
 ("M-]" . windmove-right)
 ("M-[" . windmove-left)

 ;; Resize windows
 ("M-s-<up>" . enlarge-window)
 ("M-s-<down>" . shrink-window)
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

 ([remap goto-line] . goto-line-with-feedback)

 :map ctl-x-4-map
 ("t" . toggle-window-split)

 :map m-window-map
 ("o" . window-config-org)
 ("e" . window-config-dotemacs)

 :map m-toggle-map
 ("e" . toggle-debug-on-error)
 ("g" . toggle-debug-on-quit)
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
     (setcdr (cdr args) nil)
     args))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match replacement))))

;; Show line in the original buffer from occur mode
(custom-set-variables '(list-matching-lines-jump-to-current-line t))

(use-package imenu
  :config
  (defun imenu-goto-item (direction)
    "Jump to the next or previous imenu item, depending on DIRECTION.

If direction is 1, jump to next imenu item. If direction is -1,
jump to previous imenu item.

See https://emacs.stackexchange.com/questions/30673. Adapted from
`which-function' in
https://github.com/typester/emacs/blob/master/lisp/progmodes/which-func.el."
    (let ((alist (or imenu--index-alist (imenu--make-index-alist t)))
          (minoffset (point-max))
          offset pair mark imstack destination)
      ;; Elements of alist are either ("name" . marker), or
      ;; ("submenu" ("name" . marker) ... ). The list can be
      ;; arbitrarily nested.
      (while (or alist imstack)
        (if alist
            (progn
              (setq pair (car-safe alist)
                    alist (cdr-safe alist))
              (cond ((atom pair))       ; skip anything not a cons
                    ((imenu--subalist-p pair)
                     (setq imstack   (cons alist imstack)
                           alist     (cdr pair)))
                    ((number-or-marker-p (setq mark (cdr pair)))
                     (if (> (setq offset (* (- mark (point)) direction)) 0)
                         (if (< offset minoffset) ; find the closest item
                             (setq minoffset offset
                                   destination mark))))))
          (setq alist     (car imstack)
                imstack   (cdr imstack))))
      (when destination (imenu-default-goto-function "" destination ""))))

  (defun imenu-goto-next ()
    (interactive)
    (imenu-goto-item 1))

  (defun imenu-goto-previous ()
    (interactive)
    (imenu-goto-item -1))

  :bind
  ("C-c i p" . imenu-goto-previous)
  ("C-c i n" . imenu-goto-next)
  ("C-'" . imenu))

;; TODO Make `mini-frame-mode' play nice with `window-highlight-mode'.
;; (use-package mini-frame
;;   :demand t
;;   :custom
;;   (mini-frame-show-parameters '((top . 10) (width . 0.9) (left . 0.5)))
;;   :config
;;   (mini-frame-mode))

(use-package selectrum
  :demand t

  :preface
  (defvar selectrum-swiper-history nil "Submission history for `selectrum-swiper'.")
  (autoload 'selectrum--read "selectrum")

  ;; TODO
  ;; In the process of adapting this from https://github.com/raxod502/selectrum/wiki/Useful-Commands#swiper-like-jumping-to-matching-lines.
  (defun selectrum-swiper (&optional initial-input)
    "Search for a matching line and jump to the beginning of its text.
The default candidate is a non-empty line closest to point.
This command obeys narrowing."
    (interactive)
    (let ((selectrum-should-sort nil)
          ;; Get the current line number for determining the travel distance.
          (current-line-number (line-number-at-pos (point) t)))
      (cl-destructuring-bind (default-candidate formatted-candidates)
          (cl-loop
           with buffer-lines = (split-string (buffer-string) "\n")
           with number-format = (concat "%0"
                                        (number-to-string
                                         (length (number-to-string
                                                  (length buffer-lines))))
                                        "d: ")

           with formatted-candidates = nil
           for line-text in buffer-lines
           for line-num = (line-number-at-pos (point-min) t) then (1+ line-num)

           with default-candidate = nil
           with prev-distance-to-default-cand = 1.0e+INF ; This updated later.
           for distance-to-default-cand = (abs (- current-line-number line-num))

           unless (string-empty-p line-text) ; Just skip empty lines.
           do
           ;; Find if we’ve started to move away from the current line.
           (when (null default-candidate)
             (when (> distance-to-default-cand
                      prev-distance-to-default-cand)
               (setq default-candidate (cl-first formatted-candidates)))
             (setq prev-distance-to-default-cand distance-to-default-cand))

           ;; Format current line and collect candidate.
           (push (propertize (concat (propertize (format number-format line-num)
                                                 'face 'completions-annotations)
                                     line-text)
                             'line-num line-num)
                 formatted-candidates)

           finally return (list default-candidate
                                (nreverse formatted-candidates)))
        (let ((chosen-line-number
               (get-text-property
                0 'line-num
                ;; (completing-read "Jump to matching line: "
                ;;                  formatted-candidates
                ;;                  nil t nil 'selectrum-swiper-history
                ;;                  default-candidate))))

                (selectrum--read "Jump to matching line: "
                                 formatted-candidates
                                 :default-candidate default-candidate
                                 :history 'selectrum-swiper-history
                                 :require-match t))))
          (push-mark (point) t)
          (forward-line (- chosen-line-number current-line-number))
          (beginning-of-line-text 1)))))

;;   (defun embark-export-selectrum-swiper (lines)
;;     "Create an occur mode buffer listing LINES.
;; Adapted from `embark-consult-export-occur'."
;;     (message "Exporting lines: \n%S" lines)
;;     (let ((buf (generate-new-buffer "*Embark Export Occur*"))
;;           (mouse-msg "mouse-2: go to this occurrence")
;;           last-buf)
;;       (with-current-buffer buf
;;         (dolist (line lines)
;;           (let ((num (get-text-property 0 'line-num))
;;                 ;; the text properties added to the following strings are
;;                 ;; taken from occur-engine
;;                 (lineno (propertize (format "%7d:" num)
;;                                     'occur-prefix t
;;                                     ;; Allow insertion of text at the end
;;                                     ;; of the prefix (for Occur Edit mode).
;;                                     'front-sticky t
;;                                     'rear-nonsticky t
;;                                     'occur-target loc
;;                                     'follow-link t
;;                                     'help-echo mouse-msg))
;;                 (contents (propertize (embark-consult--strip line)
;;                                       'occur-target loc
;;                                       'occur-match t
;;                                       'follow-link t
;;                                       'help-echo mouse-msg))
;;                 (nl (propertize "\n" 'occur-target loc))
;;                 (this-buf (marker-buffer loc))
;;                 (unless (eq this-buf last-buf)
;;                   (insert (propertize
;;                            (format "lines from buffer: %s\n" this-buf)
;;                            'face list-matching-lines-buffer-name-face))
;;                   (setq last-buf this-buf))
;;                 (insert (concat lineno contents nl))))
;;           (goto-char (point-min))
;;           (occur-mode))
;;         (pop-to-buffer buf))))

;;   (with-eval-after-load 'embark
;;     (add-to-list 'embark-exporters-alist
;;                  '(selectrum-swiper . embark-export-selectrum-swiper)))

  ;; Stolen from https://github.com/raxod502/selectrum/wiki/Additional-Configuration#minibuffer-default-add-function.
  (autoload 'ffap-guesser "ffap")

  (defun minibuffer-default-add-function+ ()
    (with-selected-window (minibuffer-selected-window)
      (delete-dups
       (delq nil
             (list (thing-at-point 'symbol)
                   (thing-at-point 'list)
                   (ffap-guesser)
                   (thing-at-point-url-at-point))))))

  (setq minibuffer-default-add-function #'minibuffer-default-add-function+)

  ;; Stolen from
  ;; https://github.com/raxod502/selectrum/wiki/Additional-Configuration#complete-file-names-at-point
  (autoload 'ffap-file-at-point "ffap")

  (defun complete-path-at-point+ ()
    (let ((fn (ffap-file-at-point))
          (fap (thing-at-point 'filename)))
      (when (and (or fn
                     (equal "/" fap))
                 (save-excursion
                   (search-backward fap (line-beginning-position) t)))
        (list (match-beginning 0)
              (match-end 0)
              #'completion-file-name-table))))

  (add-hook 'completion-at-point-functions #'complete-path-at-point+ 'append)

  :custom
  ;; Don't move the default candidate to the top of the list.  This is important
  ;; for `counsel-line' to preserve line order. See
  ;; `consult-line-start-from-top'.
  (selectrum-move-default-candidate nil)

  :config
  (selectrum-mode)

  :bind
  ("C-c C-r" . selectrum-repeat)
  ("s-f" . selectrum-swiper))


(use-package selectrum-prescient
  :after selectrum
  :demand t
  :config
  (selectrum-prescient-mode)
  (prescient-persist-mode))

;; FIXME `consult-man' doesn't show any results for `rg' because it's only two
;; characters.
(use-package consult
  :defer 2

  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :preface
  (defun consult-swiper (&optional initial start)
    "A `swiper' clone for `consult'.
Optional INITIAL input can be provided.
The search starting point is changed if the START prefix argument is set.
The symbol at point and the last `isearch-string' is added to the future history."
    (interactive (list nil (not (not current-prefix-arg))))
    (let ((candidates (consult--with-increased-gc
                       (consult--line-candidates
                        (not (eq start consult-line-start-from-top))))))
      (consult--read
       candidates
       :prompt "Go to line: "
       :annotate (consult--line-prefix)
       :category 'consult-location
       :sort nil
       :require-match t
       ;; Always add last isearch string to future history
       :add-history (list (thing-at-point 'symbol) isearch-string)
       :history '(:input consult--line-history)
       :lookup #'consult--line-match
       :default (caddr candidates)
       ;; Add isearch-string as initial input if starting from isearch
       :initial (or initial
                    (and isearch-mode (prog1 isearch-string (isearch-done))))
       :state (consult--jump-state))))

  :custom
  (consult-line-start-from-top 'start)
  (consult-narrow-key "<")
  (consult-find-command "fd --color=never --full-path ARG OPTS")

  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  ;; FIXME Don't preview buffer when using TRAMP and `consult-buffer'.  This
  ;; seems like a band-aid, not a real solution.
  (defun consult-buffer-state-no-tramp ()
    "Buffer state function that doesn't preview Tramp buffers."
    (let ((orig-state (consult--buffer-state))
          (filter (lambda (cand restore)
                    (if (or restore
                            (let ((buffer (get-buffer cand)))
                              (and buffer
                                   (not (file-remote-p (buffer-local-value 'default-directory buffer))))))
                        cand
                      nil))))
      (lambda (cand restore)
        (funcall orig-state (funcall filter cand restore) restore))))

  (setq consult--source-buffer
        (plist-put consult--source-buffer :state #'consult-buffer-state-no-tramp))

  ;; TODO Port this to `consult'.
  ;; (defun counsel-switch-buffer-by-mode (mode)
  ;;   "Choose a major MODE, then select from buffers of that mode."
  ;;   (interactive)
  ;;   (list (ivy-read "Choose buffers for major mode: "
  ;;                   (list-buffer-major-modes)
  ;;                   :history 'switch-to-buffer-by-mode-history
  ;;                   :action 'counsel-switch-buffer-by-mode))
  ;;   (when (stringp mode) (setq mode (intern mode)))
  ;;   (let ((buffers (mapcar #'buffer-name (filter-buffers-by-mode mode))))
  ;;     (ivy-read (format "%s buffers: " mode) buffers
  ;;               :keymap ivy-switch-buffer-map
  ;;               :action #'ivy--switch-buffer-action
  ;;               :matcher #'ivy--switch-buffer-matcher
  ;;               :preselect (when (eq major-mode mode) (cadr buffers))
  ;;               ;; Use the `ivy-switch-buffer' actions.
  ;;               :caller #'ivy-switch-buffer)))

  :bind
  ;; C-c bindings (mode-specific-map)
  ("C-c h" . consult-history)
  ("C-c m" . consult-mode-command)
  ("C-c b" . consult-bookmark)
  ("C-c k" . consult-kmacro)

  ;; C-x bindings (ctl-x-map)
  ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complet-command
  ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
  ("s-b" . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame

  ;; Custom M-# bindings for fast register access
  ("M-'" . consult-register-load)
  ("C-M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
  ("C-s-'" . consult-register)

  ;; Other custom bindings
  ("M-s-v" . consult-yank-pop)
  ("M-Y" . consult-yank-pop)

  ;; M-g bindings (goto-map)
  ("M-g e" . consult-compile-error)
  ("M-g g" . consult-goto-line)   ;; orig. goto-line
  ("M-g M-g" . consult-goto-line) ;; orig. goto-line
  ("M-g o" . consult-outline)
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g i" . consult-imenu)
  ("C-'" . consult-imenu)
  ("M-g I" . consult-project-imenu)

  ;; M-s bindings (search-map)
  ("M-s f" . consult-find)
  ("M-s L" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s-f" . consult-ripgrep)
  ("M-s l" . consult-line)
  ;; Using `selectrum-swiper' instead.
  ;; ("s-f" . consult-line)
  ("M-s m" . consult-multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)

  ;; Isearch integration
  ("M-s e" . consult-isearch)

  ;; Theme
  ("C-M-S-s-t" . consult-theme)
  ("M-m M-t" . consult-theme)

  ;; Recentf
  ("C-x f" . consult-recent-file)  ;; orig. set-fill-column

  (:map isearch-mode-map
        ("M-e" . consult-isearch)   ;; orig. isearch-edit-string
        ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
        ("M-s l" . consult-line)))    ;; required by consult-line to detect isearch

(use-package marginalia
  :defer 4
  :config
  (marginalia-mode))

(use-package embark
  :defer 5

  :preface
  ;; TODO Make this robust and contribute upstream.
  (defun embark-other-window (target)
    "Complete the current action in another window."
    (run-with-timer
     0 nil
     (lambda (command target)
       (switch-to-buffer-other-window (current-buffer))
       (funcall-interactively command target))
     selectrum--last-command
     target))

  (defun embark-execute-command-other-window (command)
    "Execute `command' in another window."
    (run-with-timer
     0 nil
     (lambda (command)
       (switch-to-buffer-other-window (current-buffer))
       (call-interactively (intern command)))
     command))

  :config
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Hide the mode line of the Embark live/completions buffers
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
  ;;                nil
  ;;                (window-parameters (mode-line-format . none))))

  ;; Use `which-key' as a reminder for `embark' key bindings.
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)

  :bind
  ("C-s-o" . embark-act)
  ;; Alternative for `describe-bindings'.
  ("C-h C-b" . embark-bindings)
  (:map selectrum-minibuffer-map
        ("C-o" . embark-act)
        :map embark-symbol-map
        ("h" . helpful-symbol)
        :map embark-general-map
        ("o" . embark-other-window)
        :map embark-command-map
        ("o" . embark-execute-command-other-window)
        :map embark-become-help-map
        ("v" . helpful-variable)
        ("f" . helpful-function)
        ("s" . helpful-symbol)))

(use-package embark-consult
  :after embark consult
  :defer 6
  :hook
  ;; Enable `consult' previews as you move around an auto-updating
  ;; `embark-collect' buffer.
  (embark-collect-mode-hook . embark-consult-preview-minor-mode))

(use-package ctrlf
  :defer 7
  :config
  (ctrlf-mode))

(use-package company
  :defer 5

  ;; :custom
  ;; (company-dabbrev-ignore-case t)

  :commands
  company-select-next
  company-select-previous

  :config
  (with-eval-after-load 'eldoc
    (eldoc-add-command #'company-select-next #'company-select-previous))

  (global-company-mode)

  :hook
  ((prog-mode-hook lisp-interaction-mode-hook cider-repl-mode-hook) . company-mode)
  ;; TODO: Figure out how to make company-mode work in the minibuffer.
  ;; (minibuffer-setup-hook . company-mode)

  :bind
  ("M-/" . company-complete)
  (:map company-mode-map
        ("M-/" . company-complete))
  (:map company-active-map
        ("RET" . nil)
        ("<return>" . nil)
        ("<tab>" . company-complete-selection)
        ("C-s" . company-filter-candidates)
        ("M-." . company-show-location))
  (:map minibuffer-local-map
        ("M-/" . completion-at-point))
  (:map minibuffer-local-completion-map
        ("M-/" . completion-at-point)))

(use-package company-prescient
  :hook
  (company-mode-hook . company-prescient-mode))

(use-package company-box
  :if window-system
  :after company
  :hook
  (company-mode-hook . company-box-mode))

(use-package projectile
  :custom
  (projectile-keymap-prefix (kbd "C-c p"))
  (projectile-project-search-path (list code-directory))
  (projectile-globally-ignored-files '("TAGS" "package-lock.json"))
  (projectile-switch-project-action 'projectile-dired)
  (projectile-mode-line nil)

  :config
  (defun projectile-project-root-tramp-short-circuit (f &optional dir)
    "Wrap `projectile-project-root' to short circuit TRAMP dirs."
    (unless (file-remote-p (or dir default-directory))
      (funcall f dir)))

  (advice-add 'projectile-project-root
              :around #'projectile-project-root-tramp-short-circuit)

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

  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm start"
                                    :test "npm test"
                                    :test-suffix ".test")
  (projectile-register-project-type 'clojure-cli '("deps.edn")
                                    :compile "clj "
                                    :test-suffix "_test")
  (projectile-register-project-type 'shadow-cljs '("shadow-cljs.edn")
                                    :compile "clj "
                                    :test-suffix "_test")

  (projectile-mode)

  :bind-keymap
  ("C-c p" . projectile-command-map)

  :bind
  ("s-p" . projectile-find-file)
  ("s-P" . projectile-switch-project)
  ("s-B" . projectile-switch-to-buffer)
  ("s-}" . projectile-next-project-buffer)
  ("C-c }" . projectile-next-project-buffer)
  ("s-{" . projectile-previous-project-buffer)
  ("C-c {" . projectile-previous-project-buffer)
  ("C-s-b" . projectile-switch-to-buffer))

(use-package smart-jump
  :preface
  (defun smart-jump-go-other-window (&optional smart-list continue)
    "Show the function/variable declartion for thing at point in another window.

SMART-LIST will be set (or nil) if this is a continuation of a
previous jump.

CONTINUE will be non nil if this is a continuation of a previous jump."
    (interactive)
    (let ((old (current-buffer))
          new)
      (call-interactively #'smart-jump-go smart-list continue)
      (setq new (current-buffer))
      ;; If old and new are the same then `xref' has popped up another window
      ;; listing multiple definitions and we bail.
      (unless (eq new old)
        (switch-to-buffer old)
        (switch-to-buffer-other-window new))))

  :config
  (smart-jump-setup-default-registers)

  ;; Use xref-goto-definitions in `helpful-mode'.
  (smart-jump-register :modes 'helpful-mode)

  :bind
  ("M-." . smart-jump-go)
  ("M-?" . smart-jump-references)
  ("M-P" . smart-jump-go-other-window)
  ("C-x 4 ." . smart-jump-go-other-window))

(use-package wgrep
  :custom
  ;; Save changed buffers immediately when exiting wgrep mode
  (wgrep-auto-save-buffer t)
  :bind
  (:map grep-mode-map
        ("C-c C-p" . wgrep-change-to-wgrep-mode)
        :map occur-mode-map
        ("C-c C-p" . wgrep-change-to-wgrep-mode)))

;; > brew install ripgrep
(use-package rg
  :config
  (rg-enable-default-bindings (kbd "M-s M-r")))

(use-package spotlight
  :bind
  (:map m-search-map
        ("s" . spotlight)
        ("S" . spotlight-fast)))

(bind-key "s-5" #'replace-regexp-entire-buffer-immediately)


;;;; File Management

;; Customize `files'.
(custom-set-variables
 '(remote-file-name-inhibit-cache nil))

(use-package epg
  :custom
  (epg-pinentry-mode 'loopback))

(use-package pinentry
  :config
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  (pinentry-start))

(defun dos-to-unix ()
  "Convert DOS line endings to Unix ones."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t)))
  (set-buffer-file-coding-system 'unix 't))

(defun unix-to-dos ()
  "Convert Unix encoded buffer to DOS encoding.
https://edivad.wordpress.com/2007/04/03/emacs-convert-dos-to-unix-and-vice-versa/"
  (interactive)
  (set-buffer-file-coding-system 'dos))

(defvar touch-history nil
  "History for `touch' command.")

(defun touch (args)
  "Run `touch ARGS' in `default-directory'."
  (interactive (list (read-shell-command "touch " nil 'touch-history)))
  (async-shell-command (format "touch %s" args)))

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

(defmacro expand-file-name* (&rest names)
  "Like `expand-file-name' but expands more than two NAMES."
  (let* ((names (reverse names))
         (expr `(expand-file-name ,(pop names))))
    (while (> (length names) 0)
      (setq expr `(expand-file-name ,(pop names) ,expr)))
    expr))

(use-package x509-mode
  :straight (x509-mode :host github :repo "mnewt/x509-mode"))

(defun unison-sync (command)
  "Run a Unison sync of files using COMMAND."
  (let ((buffer (get-buffer-create "*unison-sync*")))
    (with-current-buffer buffer
      (setq-local comint-output-filter-functions
                  '(comint-postoutput-scroll-to-bottom
                    comint-watch-for-password-prompt
                    (lambda (_) (font-lock-ensure))))
      (async-shell-command command buffer)
      (font-lock-add-keywords
       nil
       '(("^\\(\\[\\(?:BGN\\|END\\)\\]\\) \\(.*\\)$"
          (1 font-lock-builtin-face)
          (2 font-lock-type-face))
         ("^Failed: .*" . 'compilation-error)
         ("^Synchronization .*" . 'font-lock-keyword-face)
         ("^\\(?:Unison\\|UNISON\\|Connected\\|Looking\\|Reconciling\\|Propagating\\|Saving\\|Nothing\\|  Waiting\\) .*" . 'font-lock-comment-face))))))


;;;;; psync (https://github.com/mnewt/psync)

(add-to-list 'auto-mode-alist '("psync_config\\'" . sh-mode))

(defvar-local psync-directory nil
  "Cached directory for `psync'.

It is always buffer local.")

(defun psync-maybe ()
  "If we find a `psync_config' file then run `psync'.

See: https://github.com/mnewt/psync"
  (interactive)
  (when-let ((default-directory (and (not (file-remote-p default-directory))
                                     (locate-dominating-file default-directory
                                                             "psync_config"))))
    (setq psync-directory default-directory)
    (if (= 0 (call-process-shell-command "psync"))
        (message "psync complete in directory %s." default-directory)
      (error "Synchronization with psync failed in directory: %s"
             default-directory))))

(defun psync-clone (local remote)
  "Clone a new repository for use with `psync' from LOCAL to REMOTE."
  (interactive (list (read-directory-name "Local directory: ")
                     (read-directory-name "Remote directory: ")))
  (async-shell-command (format "psync -v clone '%s' '%s'" local remote)))

(add-hook 'after-save-hook #'psync-maybe)


;;;; OS program interaction

(use-package server
  :defer 3
  :config
  (unless (server-running-p)
    (server-start)))

(defun reveal-in-windows-explorer (&optional file)
  "Reveal FILE in Windows Explorer."
  (call-process "explorer" nil 0 nil
                (concat "/select," (dired-replace-in-string
                                    "/" "\\"
                                    (or file
                                        buffer-file-name
                                        (dired-get-file-for-visit))))))

(use-package reveal-in-osx-finder
  :straight (reveal-in-osx-finder :type git :host github
                                  :repo "mnewt/reveal-in-osx-finder")
  :if (eq system-type 'darwin))

;; > brew install trash
(use-package osx-trash
  :if (eq system-type 'darwin)
  :config
  (setq delete-by-moving-to-trash t)
  (osx-trash-setup))

(defun reveal-file (&optional _file)
  "Reveal FILE using the operating system's GUI file browser."
  (interactive)
  (cl-case system-type
    (darwin (reveal-in-osx-finder))
    (windows-nt (reveal-in-windows-explorer))
    (cygwin (reveal-in-windows-explorer))))

(defun os-open-file (&optional file)
  "Open visited FILE in default external program.
When in dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
  (interactive)
  (let* ((file (if (eq major-mode 'dired-mode)
                   (dired-get-file-for-visit)
                 (or file buffer-file-name)))
         (open (cl-case system-type
                 (darwin "open")
                 ((gnu gnu/linux gnu/kfreebsd) "xdg-open")
                 ((windows-nt cygwin) "command")))
         (program (if (or current-prefix-arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (message "Opening %s in the OS registered external program..." file)
    (call-process program nil 0 nil file)))

(defun brew-prefix (package)
  "Get the `homebrew' install prefix for PACKAGE."
  (shell-command-to-string (format "printf %%s \"$(brew --prefix %s)\"" package)))

(bind-keys
 ("C-c O" . reveal-file))


;;;; Dired

(use-package dired-rainbow
  :defer 4
  :config
  (dired-rainbow-define-chmod directory "#0074d9" "d.*")
  (dired-rainbow-define html "#eb5286"
                        ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht"
                         "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024"
                        ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg"
                         "pgn" "rss" "yaml" "yml" "rdata" "sln" "csproj"
                         "meta" "unity" "tres" "tscn" "import" "godot"))
  (dired-rainbow-define document "#9561e2"
                        ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps"
                         "rtf" "djvu" "epub" "odp" "ppt" "pptx" "xls" "xlsx"
                         "vsd" "vsdx" "plantuml"))
  (dired-rainbow-define markdown "#4dc0b5"
                        ("org" "org_archive" "etx" "info" "markdown" "md"
                         "mkd" "nfo" "pod" "rst" "tex" "texi" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd"
                        ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f"
                        ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv"
                         "ogg" "mov" "mid" "midi" "wav" "aiff" "flac" "mkv"))
  (dired-rainbow-define image "#f66d9b"
                        ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png"
                         "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11"
                        ("log" "log.1" "log.2" "log.3" "log.4" "log.5" "log.6"
                         "log.7" "log.8" "log.9"))
  (dired-rainbow-define shell "#f6993f"
                        ("awk" "bash" "bat" "fish" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172"
                        ("py" "ipynb" "hy" "rb" "pl" "t" "msql" "mysql"
                         "pgsql" "sql" "r" "clj" "cljs" "cljc" "cljx" "edn"
                         "scala" "js" "jsx" "lua" "fnl" "gd"))
  (dired-rainbow-define compiled "#6cb2eb"
                        ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp"
                         "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn"
                         "f90" "f95" "f03" "f08" "s" "rs" "active" "hs"
                         "pyc" "java"))
  (dired-rainbow-define executable "#8cc4ff"
                        ("com" "exe" "msi"))
  (dired-rainbow-define compressed "#51d88a"
                        ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar"
                         "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar" "rar"))
  (dired-rainbow-define packaged "#faad63"
                        ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf"
                         "vpk" "bsp"))
  (dired-rainbow-define encrypted "#f2d024"
                        ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12"
                         "pem"))
  (dired-rainbow-define fonts "#f6993f"
                        ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf" "woff"
                         "woff2" "eot"))
  (dired-rainbow-define partition "#e3342f"
                        ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk"
                         "bak"))
  (dired-rainbow-define vc "#6cb2eb"
                        ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define config "#5040e2"
                        ("cfg" "conf"))
  (dired-rainbow-define certificate "#6cb2eb"
                        ("cer" "crt" "pfx" "p7b" "csr" "req" "key"))
  (dired-rainbow-define junk "#7F7D7D"
                        ("DS_Store" "projectile"))
  (dired-rainbow-define icloud "#e3342f" ("icloud"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")

  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (equal major-mode 'dired-mode)
        (font-lock-refresh-defaults)))))

(use-package dired-rainbow-listing
  :straight (:type git :host github :repo "mnewt/dired-rainbow-listing")
  :hook
  (dired-mode-hook . dired-rainbow-listing-mode))

(use-package dired
  :straight (:type built-in)
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-dwim-target t)
  ;; `dired-omit-mode' is managed by `dired-filter'.
  ;; (dired-omit-mode t)
  (dired-omit-files "\\`\\(?:[#.]\\|flycheck_\\).*")
  ;; Don't prompt to kill buffers of deleted directories.
  (find-ls-option '("-print0 | xargs -0 ls -alhd" . ""))

  ;; Also customize `dired-x'.
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-bind-man nil)
  (dired-bind-info nil)

  :config
  ;; Set it here because inside :custom it overrides defer
  (setq dired-listing-switches "-aFhl"
        ;; Try to use GNU ls on macOS since BSD ls doesn't explicitly support
        ;; Emacs and can run into issues with certain characters in the file name.
        insert-directory-program (or (executable-find "gls")
                                     (executable-find "ls")))
  (defun dired-open-file ()
    "Open file at point in OS default program."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (os-open-file file)))

  :hook
  (dired-mode-hook . dired-hide-details-mode)
  :bind
  ("C-x M-s" . psync-maybe)
  ("C-c o" . os-open-file)
  (:map m-toggle-map
        ("r" . auto-revert-mode))
  (:map dired-mode-map
        ("C-c C-o" . dired-open-file)
        ("T" . touch)
        ("F" . tail-file)
        (";" . dired-git-add)
        ("C-c C-p" . wdired-change-to-wdired-mode)
        ("C-c C-r" . dired-rsync)
        ("N" . dired-narrow)
        ("I" . dired-subtree-cycle)
        ("TAB" . dired-subtree-cycle)
        ("C-, i" . dired-subtree-insert)
        ("C-, r" . dired-subtree-remove)
        ("C-, R" . dired-subtree-revert)
        ("C-, n" . dired-subtree-narrow)
        ("C-, ^" . dired-subtree-up)
        ("C-, v" . dired-subtree-down)))

;; TODO Pop open status buffer
;; TODO Fix status buffer's display
;; TODO Send PR, reference:
;; https://github.com/stsquad/dired-rsync/issues/12
(use-package dired-rsync
  :after dired
  :commands
  dired-rsync-backup
  :config
  (defun dired-rsync--set-mode-line-misc-info (&optional _err _ind)
    "Put `dired-rsync-modeline-status' in `mode-line-misc-info'.

ERR and IND are ignored."
    (setf (alist-get 'dired-rsync mode-line-misc-info)
          (list dired-rsync-modeline-status)))

  (advice-add #'dired-rsync--update-modeline
              :after #'dired-rsync--set-mode-line-misc-info)

  (defun dired-rsync-backup--add-version (file)
    "Return FILE modified with a version string."
    (let ((extension (file-name-extension file)))
      (concat (file-name-directory file) (file-name-base file)
              "-" (format-time-string "%Y-%m-%d")
              (when extension (concat "." extension)))))

  (defun dired-rsync-backup (dest)
    "Like `dired-rsync' but appends a timestamp to DEST."
    (interactive (list (read-file-name "Backup to: " (dired-dwim-target-directory)
                                       nil nil nil #'file-directory-p)))
    (let* ((sfiles (funcall dired-rsync-source-files))
           (backup-name (if (cdr sfiles)
                            ;; If backing up multiple files, create a directory
                            ;; at the destination with the same name as the
                            ;; parent of the first source file.
                            (expand-file-name (file-name-nondirectory
                                               (directory-file-name
                                                (file-name-directory (car sfiles))))
                                              dest)
                          ;; If backing up a single file, create a file at the
                          ;; destination.
                          (car sfiles)))
           extension file)
      (dired-rsync (dired-rsync-backup--add-version backup-name)))))

(use-package disk-usage
  :after dired
  :bind (:map dired-mode-map
              (")" . disk-usage-here)))

(use-package wdired
  :after dired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t)
  :commands
  wdired-change-to-wdired-mode)

(use-package dired-narrow
  :after dired
  :commands
  dired-narrow)

(use-package dired-filter
  :after dired
  :custom
  (dired-filter-verbose nil)
  (dired-filter-prefix "/")
  :hook
  (dired-mode-hook . dired-filter-mode))

(use-package dired-subtree
  :after dired)

(use-package dired-collapse
  :after dired
  :hook
  (dired-mode-hook . dired-collapse-mode))

(use-package dired-list
  :after dired
  :straight (dired-list :host github :repo "Fuco1/dired-hacks"
                        :files ("dired-list.el")))

(use-package dired-quick-sort
  :after dired)

(use-package dired-git-info
  :after dired
  :bind
  (:map dired-mode-map
        ("_" . dired-git-info-mode)))

(use-package dired-sidebar
  :after dired
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  :hook
  (dired-sidebar-mode-hook . (lambda ()
                               (unless (file-remote-p default-directory)
                                 (auto-revert-mode))))
  :bind
  ("C-x C-d" . dired-sidebar-toggle-sidebar))


;;;; Help

;; Help and Documentation lookup

;; Get rid of prompting for disabled commands.
(setq disabled-command-function nil)

;; Change yes/no prompts to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Customize `simple'.
(custom-set-variables
 '(shell-command-prompt-show-cwd t)
 '(suggest-key-bindings 5))

;; Enable all commands without warnings.
(setq disabled-command-function nil)

(use-package helpful
  :preface
  (defun push-button-other-window ()
    "Like push button but opens in other window."
    (interactive)
    (let ((new-buffer (save-window-excursion
                        (push-button)
                        (current-buffer))))
      ;; Don't use `pop-to-buffer' because we are actively trying to ignore
      ;; `display-buffer-alist' and pop to a new buffer no matter what.
      (other-window 1)
      (switch-to-buffer new-buffer)))

  :config
  (set-face-attribute 'helpful-heading nil :inherit 'outline-2)

  :bind
  ("C-h ." . helpful-at-point)
  ("C-h f" . helpful-callable)
  ("C-h c" . helpful-command)
  ("C-h F" . helpful-function)
  ("C-h k" . helpful-key)
  ("C-h M" . helpful-macro)
  ("C-h o" . helpful-symbol)
  ("C-h v" . helpful-variable)
  ("C-h C-k" . helpful-keymap)
  (:map helpful-mode-map
        ("<tab>" . forward-button)
        ("M-p" . imenu-goto-previous)
        ("M-n" . imenu-goto-next)
        ("o" . push-button-other-window)
        ("C-x C-e" . eval-last-sexp)
        ("s-<return>" . eval-last-sexp)))

(use-package eldoc
  :defer 20
  :config
  (global-eldoc-mode))

(use-package eldoc-box
  :after eldoc

  :custom
  (eldoc-box-clear-with-C-g t)

  :config
  ;; Display the message using up to 90% of the frame height.
  (custom-set-variables '(eldoc-echo-area-use-multiline-p 0.9))

  ;; Customize `eldoc' setup for Emacs Lisp to display docstrings in addition to
  ;; the usual signature.
  (with-eval-after-load 'elisp-mode
    (defun elisp--fnsym-add-docstring (f sym &optional index)
      "Add the docstring for SYM to `eldoc'.
See `elisp-get-fnsym-args-string'."
      (when-let ((string (funcall f sym index))
                 (doc (propertize (documentation sym t) 'face 'font-lock-doc-face)))
        (concat string "\n" doc)))

    (advice-add #'elisp-get-fnsym-args-string :around #'elisp--fnsym-add-docstring)

    (defun elisp--var-full-docstring (f sym)
      "Modify `elisp-get-var-docstring' to return the full docstring."
      (cl-letf (((symbol-function #'elisp--docstring-first-line)
                 (lambda (doc) doc)))
        (when-let ((doc (funcall f sym)))
          (propertize doc 'face 'font-lock-doc-face))))

    (advice-add #'elisp-get-var-docstring :around #'elisp--var-full-docstring))

  :hook
  (eldoc-mode-hook . eldoc-box-hover-mode))

(use-package which-key
  :defer 19
  :custom
  ;; Fix misalignment, probably due to fallback font being used for the arrows.
  ;; Also the non-unicode colons are more pleasing anyway.
  (which-key-dont-use-unicode t)
  (which-key-idle-delay 0.25)
  :config
  (defun which-key-M-x-prefix+ (&optional _)
    "Completing read and execute command from current prefix map.

This command can be used as `prefix-help-command'.

The optional argument is ignored and only for compatability with
`which-key-C-h-dispatch' so this command can be bound in
`which-key-C-h-map', too."
    (interactive)
    (let* ((evs (if (which-key--current-prefix)
                    (which-key--current-key-list)
                  (butlast (append (this-command-keys-vector) nil))))
           (key (apply #'vector evs))
           (map (key-binding key)))
      (which-key--execute-binding+ map (key-description key))))

  (defun which-key--execute-binding+ (map &optional prefix)
    "Completing read command from MAP and execute it.

If PREFIX is given it should be a key description which will be
included in the prompt."
    (let ((cmd (which-key--completing-read-cmd+ map prefix)))
      (when (commandp cmd)
        (which-key--execute-cmd+ cmd))))

  (defun which-key--completing-read-cmd+ (map &optional prefix)
    "Completing read command from MAP.

Include PREFIX in prompt if given."
    (which-key--hide-popup-ignore-command)
    (let* ((desc
            (completing-read
             (if prefix
                 (format "Execute (%s): " prefix)
               "Execute: ")
             (mapcar #'which-key--completing-read-format+
                     (which-key--get-keymap-bindings map 'all)))))
      (intern (car (split-string desc)))))

  (defun which-key--execute-cmd+ (cmd)
    "Execute command CMD as if invoked by key sequence."
    (setq prefix-arg current-prefix-arg)
    (setq this-command cmd)
    (setq real-this-command cmd)
    (command-execute cmd 'record))

  (defun which-key--completing-read-format+ (bnd)
    "Format binding BND for `completing-read'."
    (let* ((key (car bnd))
           (cmd (cdr bnd))
           (desc (format "%s (%s)" cmd
                         (propertize key 'face 'which-key-key-face))))
      (which-key--maybe-add-docstring
       (format "%-50s" desc) cmd)))

  ;; https://with-emacs.com/posts/prefix-command-completion/
  (setq prefix-help-command #'which-key-M-x-prefix+)

  (which-key-mode)

  :bind
  ("C-s-h" . which-key-show-top-level))

(use-package man
  :custom
  ;; Make the manpage the current buffer in the other window
  (Man-notify-method 'aggressive)
  :config
  (set-face-attribute 'Man-overstrike nil
                      :inherit font-lock-type-face :weight 'bold :height 1.1)
  (set-face-attribute 'Man-underline nil
                      :inherit font-lock-keyword-face :underline t)
  :bind
  ("C-h C-m" . man))

(use-package woman
  :preface
  ;; TODO Make this usable by:
  ;;   1. Search the remote MANPATH for the file
  (defun tramp-aware-woman (man-page-path)
    "Open a remote man page at MAN-PAGE-PATH via TRAMP."
    (interactive)
    (require 'tramp)
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

  :bind
  ("C-h M-m" . woman))

(use-package info-colors
  :hook
  (Info-selection-hook . info-colors-fontify-node))

;; TODO Fix `eg.el'.
;; > pip install eg
;; (use-package eg.el
;;   :straight (eg.el :host github :repo "mnewt/eg.el")
;;   :bind
;;   ("C-h C-e" . eg))

(use-package tldr
  :bind
  ("C-h t" . tldr))

(use-package consult-dash
  :straight (consult-dash :host github :repo "canatella/consult-dash")

  :preface
  (defcustom dash-docs-docset-modes
    '((emacs-lisp-mode . "Emacs Lisp")
      (lisp-interaction-mode . "Emacs Lisp")
      (clojure-mode . "Clojure")
      (clojurescript-mode . "ClojureScript")
      (js-mode . "JavaScript")
      (csharp-mode . "Unity 3D"))
    "Alist mapping major modes to docsets.
If the current major mode is in this list, scope the search to
the corresponding docset."
    :type 'list
    :group 'dash-docs)

  (defvar dash-docs-docset-modes)

  (defun consult-dash-with-docset (docset &optional initial)
    "Query dash DOCSET.
INITIAL will be used as the initial input, if given."
    (interactive (list (assoc-default major-mode dash-docs-docset-modes)))
    (when docset
      (setq initial (concat docset " " initial)))
    (consult-dash initial))

  (defun dash-docs-update-docsets-var (&rest _)
    "Update `dash-docs-common-docsets' variable."
    (setq dash-docs-common-docsets (dash-docs-installed-docsets))
    (dash-docs-reset-connections))

  (defun dash-docs-update-all-docsets ()
    "Update all official and unofficial docsets."
    (interactive)
    (pop-to-buffer (get-buffer-create "*dash-docs updates*"))
    (erase-buffer)
    (insert "Updating Dash Docs\n==================\n\n")
    (let ((official-docsets (dash-docs-official-docsets))
          (unofficial-docsets (mapcar 'car (dash-docs-unofficial-docsets))))
      (dolist (d (mapcar (lambda (s) (replace-regexp-in-string " " "_" s))
                         (dash-docs-installed-docsets)))
        (insert (propertize (concat"  " d ": ") 'face 'bold))
        (cond
         ((member d official-docsets)
          (progn (insert "Updating official docset...\n")
                 (dash-docs-install-docset d)))
         ((member d unofficial-docsets)
          (progn (insert "Updating unofficial docset...\n")
                 (dash-docs-install-user-docset d)))
         (t (insert "Skipping manually installed docset...\n")))))
    (dash-docs-update-docsets-var)
    (insert "\n\ndone."))

  :custom
  (dash-docs-browser-func #'eww-other-window)
  (dash-docs-enable-debugging nil)
  (dash-docs-docsets-path "~/.config/docsets")

  :config
  (make-directory dash-docs-docsets-path t)

  (advice-add 'dash-docs--install-docset :after #'dash-docs-update-docsets-var)

  (setq dash-docs-common-docsets (dash-docs-installed-docsets))

  :bind
  ("M-s-l" . counsel-dash)
  ("C-h C-d" . counsel-dash)
  ("M-s-." . counsel-dash-at-point))

(use-package devdocs-lookup
  :straight (devdocs-lookup :host github :repo "skeeto/devdocs-lookup")
  :config
  (devdocs-setup)
  :bind
  ("C-h M-l" . devdocs-lookup))

(use-package atomic-chrome
  :defer 16
  :custom
  (atomic-chrome-default-major-mode 'markdown-mode)
  :config
  (defun atomic-chrome-switch-to-firefox ()
    "Switch to Firefox."
    (call-process "open" nil nil nil "-a" "FirefoxDeveloperEdition"))
  (atomic-chrome-start-server)
  :hook
  (atomic-chrome-edit-done-hook . atomic-chrome-switch-to-firefox))

(use-package gif-screencast
  :custom
  ;; To shut up the shutter sound of `screencapture'
  (gif-screencast-args '("-x"))
  (gif-screencast-cropping-program "mogrify")
  (gif-screencast-capture-format "ppm")
  (gif-screencast-output-directory (expand-file-name "~/Downloads"))
  :functions
  git-screencast--cropping-region
  :config
  ;; FIXME: https://gitlab.com/ambrevar/emacs-gif-screencast/issues/14
  ;; Double the size of the window because the cropping region is not
  ;; calculated correctly on hi-res displays like retina macbooks.
  (advice-add
   #'gif-screencast--cropping-region
   :around
   (lambda (oldfun &rest r)
     (apply #'format "%dx%d+%d+%d"
            (mapcar
             (lambda (x) (* 2 (string-to-number x)))
             (split-string (apply oldfun r) "[+x]")))))
  :commands
  gif-screencast)

;; SICP in Info Format.
;; (use-package sicp
;;   :defer t)

(bind-keys
 ("C-h C-i" . elisp-index-search)
 ("C-h M-i" . info-apropos)
 ("C-h C-l" . find-library))


;;;; Calendar and Journal

(use-package calendar
  :commands
  calendar-current-date
  calendar-gregorian-from-absolute
  new-journal-entry
  calendar-insert-date
  calendar-choose-date
  :config
  (defun calendar-iso8601-date-string (date)
    "Create an ISO8601 date string from DATE."
    (cl-destructuring-bind (month day year) date
      (format "%04i-%02i-%02i" year month day)))

  (defun calendar-date-add-days (date days)
    "Add DAYS to DATE."
    (calendar-gregorian-from-absolute (+ (calendar-absolute-from-gregorian date) days)))

  (defun calendar-choose-date ()
    "Interactively choose DATE and return it as an ISO 8601 string."
    (let* ((today (calendar-current-date))
           (day-offsets '(7 6 5 4 3 2 1 0 -1 -2 -3 -4 -5 -6 -7))
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
  :bind
  (:map m-insert-map
        ("d" . calendar-insert-date)
        ("t" . calendar-insert-date-today))
  (:map m-map
        ("C-j" . journal-new-entry)))


;;;; Mail

;; (require 'm-mail)


;;;; Math

;; Math utilities.

(use-package calc
  :config
  (defvar math-additional-units)
  (setq math-additional-units
        '((bit nil "Bit")
          (bits "bit" "Bits")
          (b "bit" "Bits")
          (bps "bit / s" "Bits per second")
          (Kib "1024 * b" "Kilo Bit")
          (Mib "1024 * Kib" "Mega Bit")
          (Gib "1024 * Mib" "Giga Bit")
          (byte "8 * bit" "Byte")
          (bytes "byte" "Bytes")
          (B "byte" "Bytes")
          (KiB "1024 * B" "Kilo Byte")
          (MiB "1024 * KiB" "Mega Byte")
          (GiB "1024 * MiB" "Giga Byte")))
  :bind
  (:map calc-mode-map
        ("s-z" . calc-undo)
        ("s-v" . calc-yank)))


;;;; Network

;; Network utilities.

(use-package pcap-mode
  :mode "\\.pcap\\'")

(use-package erc
  :hook
  ;; Automate communication with services, such as nicserv.
  (erc-connect-pre-hook . erc-services-mode))

(defun dis (hostname)
  "Resolve a HOSTNAME to its IP address."
  (interactive "MHostname: ")
  (message
   (kill-new
    (shell-command-to-string
     (concat (or (executable-find "drill")
                 (executable-find "dig"))
             " "
             hostname
             " | awk '/;; ANSWER SECTION:/{flag=1;next}/;;/{flag=0}flag'")))))

(defun ips ()
  "Show the machine's IP addresses."
  (interactive)
  (shell-command
   (cl-case system-type
     (gnu/linux
      "ip address show | awk '/inet /{if ($5 != \"inactive\") { print $7 \": \" $2 }}'")
     (darwin
      "/sbin/ifconfig | awk '/^[a-z0-9]+:/{ i=$1 } /inet / { if (i != \"lo0:\") { print i \" \" $2 }}'")
     (cygwin
      "ipconfig | awk -F' .' '/Address/ {print $NF}'"))))

(defun public-ip ()
  "Display the local host's apparent public IP address."
  (interactive)
  (url-retrieve "https://diagnostic.opendns.com/myip"
                (lambda (_)
                  (goto-char (point-min))
                  (re-search-forward "^$")
                  (delete-char 1)
                  (delete-region (point) (point-min))
                  (message (kill-new (buffer-string))))))

(defvar ipcalc-history nil
  "History for `ipcalc' command.")

(defvar ipcalc-command '("sipcalc" "--all")
  "How to run ipcalc via command line.")

(defconst ipcalc-ipv4-regexp
  "\\(?:[0-9]\\{1,3\\}\\.\\)\\{3\\}[0-9]\\{1\\}"
  "Regexp matching an IPv4 address.")

(defconst ipcalc-ipv6-regexp
  "\\(?:[0-9a-fA-F]\\{0,4\\}:\\)\\{1,7\\}\\(?:[0-9a-fA-F]\\{0,4\\}\\)?"
  "Regexp matching an IPv6 address.")

(defconst ipcalc-network-mask-regexp
  "/[0-9]\\{1,2\\}"
  "Regexp matching the network mask part of CIDR notation.")

(defconst ipcalc-cidr-regexp
  (concat "\\(?:"
          ipcalc-ipv4-regexp
          "\\|"
          ipcalc-ipv6-regexp
          "\\)"
          "\\(?:"
          ipcalc-network-mask-regexp
          "\\)?")
  "Regexp matching an IP network in CIDR notation.")

(defun ipcalc-ip-address-at-point ()
  "Return the IP address at point, or nil if there is none."
  (save-excursion
    (if (re-search-backward "[[:space:]]" (line-beginning-position) t)
        (forward-char 1)
      (beginning-of-line))
    (when (looking-at ipcalc-cidr-regexp)
      (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))

(defun ipcalc (cidr)
  "Run ipcalc on an address in CIDR format."
  (interactive (list (if (region-active-p)
                         (substring-no-properties (region-beginning) (region-end))
                       (read-shell-command "ipcalc "
                                           (ipcalc-ip-address-at-point)
                                           'ipcalc-history))))
  (async-shell-command (mapconcat #'identity (append ipcalc-command (list cidr)) " ")
                       "*ipcalc*"))

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


;;;;; mnt

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

;; Ensure `vc' stuff is not making tramp slower
(custom-set-variables
 `(vc-ignore-dir-regexp ,(format "%s\\|%s"
                                 vc-ignore-dir-regexp
                                 tramp-file-name-regexp)))

(defun git-ls-files (&optional directory)
  "Return a list of the files from `git ls-files DIRECTORY'."
  (split-string (shell-command-to-string
                 (concat "git ls-files " (or directory default-directory)))))

(defun git-add-current-file (file)
  "Run `git add' on the FILE visited in the current buffer."
  (interactive (list (buffer-file-name)))
  (let ((name (s-trim (shell-command-to-string
                       "basename -s .git $(git config --get remote.origin.url)")))
        (dir (s-trim (shell-command-to-string "git rev-parse --show-toplevel"))))
    (if (= 0 (call-process-shell-command (concat "git add " file)))
        (message "File %s was added to the git repo %s at %s."
                 (buffer-file-name) name dir)
      (error "Failed to add file %s to the git repo %s at %s"
             (buffer-file-name) name dir))))

(defun dired-git-add ()
  "Run `git add' on the selected files in a dired buffer."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (message "> git add %s" files)
    (dired-do-shell-command "git add" nil files)
    (dired-revert)))

(use-package vc
  :custom
  ;; VC follows the link and visits the real file, telling you about it in the
  ;; echo area.
  (vc-follow-symlinks t)
  ;; Backup even if file is in vc.
  (vc-make-backup-files t))

(use-package gitconfig-mode
  :mode ("/\\.gitconfig\\'" "/\\.git/config\\'" "/modules/.*/config\\'"
         "/git/config\\'" "/\\.gitmodules\\'" "/etc/gitconfig\\'"))

(use-package magit
  :custom
  (magit-repository-directories `((,code-directory . 1)))

  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch))

(use-package forge
  :after magit)

(use-package git-timemachine
  :commands
  git-timemachine)

(use-package gist
  :commands
  gist-list)

(use-package diff-mode
  :bind
  (:map diff-mode-map
        ;; Don't shadow the binding for `other-window'.
        ("M-o" . nil)
        ("M-." . diff-goto-source)))

(use-package diff-hl
  :commands
  diff-hl-magit-post-refresh
  diff-hl-mode
  diff-hl-dired-mode
  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  ((prog-mode-hook text-mode-hook) . diff-hl-mode)
  (dired-mode-hook . diff-hl-dired-mode))

(bind-keys
 ("M-m l" . git-home-link)
 ("M-m u" . git-home-unlink)
 ("C-x G" . projectile-git-ls-files-dired)
 :map m-file-map
 (";" . git-add-current-file)
 ("d" . diff-buffer-with-file))


;;;; Editing

;; General editing related configuration and functionality

;; Ensure we are always using UTF-8 encoding.
(set-charset-priority 'unicode)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix)
      locale-coding-system 'utf-8)

(setq
 ;; Use the system clipboard
 select-enable-clipboard t
 ;; Save existing system clipboard text into kill ring before replacing it,
 ;; ensuring it doesn't get irrevocably destroyed.
 save-interprogram-paste-before-kill t
 ;; use mouse to kill/yank
 mouse-yank-at-point t
 mouse-drag-and-drop-region t
 mouse-drag-and-drop-region-cut-when-buffers-differ t
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil
 ;; Newline at end of file.
 require-final-newline t
 ;; Double-spaces after periods feels wrong because it is wrong.
 sentence-end-double-space nil)

(setq-default
 ;; Wrap text.
 fill-column 80
 ;; Tabs
 indent-tabs-mode nil
 tab-width 2
 tab-stop-list (number-sequence tab-width 120 tab-width)
 ;; I only use left-to-right languages, so I can get away with disabling certain
 ;; bidirectional features to increase performance.
 bidi-paragraph-direction 'left-to-right
 bidi-inhibit-bpa t)

;; Delete selection on insert or yank
(delete-selection-mode)

;; Automatically indent after RET
(electric-indent-mode)

(defun auto-fill-mode-setup ()
  "Automatically fill comments.

Wraps on `fill-column' columns."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode t))

(add-hook 'prog-mode-hook #'auto-fill-mode-setup)

(use-package so-long
  :if (>= emacs-major-version 27)
  :defer 11
  :custom
  (so-long-threshold 500)
  :config
  (global-so-long-mode))

(use-package unfill
  :bind
  (:map prog-mode-map
        ("M-q" . unfill-toggle)))

(defun open-line-above (arg)
  "Like `open-line' only always insert ARG lines above."
  (interactive "p")
  (save-excursion (beginning-of-line arg) (open-line arg)))

(defun delete-indentation-forward (&optional arg beg end)
  "Like `delete-indentation', but in the opposite direction.
Bring the line below point up to the current line.

See another way: http://whattheemacsd.com/key-bindings.el-03.html"
  (interactive (cons current-prefix-arg
                     (and (use-region-p)
                          (list (region-beginning) (region-end)))))
  (delete-indentation (not arg) beg end))

(use-package undo-tree
  ;; `global-undo-tree-mode' must be loaded before any files are opened so that
  ;; the undo history can be restored.
  :demand t
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name "var/undo-tree" user-emacs-directory))))
  (undo-tree-enable-undo-in-region t)
  (undo-tree-visualizer-diff t)

  :config
  (add-to-list 'undo-tree-incompatible-major-modes 'vterm-mode)

  (defun undo-tree-save-history-quietly (f &rest r)
    "Cause `undo-tree' to save history without printing messages."
    (let ((inhibit-message t))
      (apply f r)))

  (advice-add 'undo-tree-save-history :around #'undo-tree-save-history-quietly)

  (defun undo-keep-region (f &optional arg)
    "Keep region when undoing in region.

Adapted from http://whattheemacsd.com/my-misc.el-02.html."
    (if (use-region-p)
        (let ((m (set-marker (make-marker) (mark)))
              (p (set-marker (make-marker) (point))))
          (funcall f arg)
          (goto-char p)
          (set-mark m)
          (set-marker p nil)
          (set-marker m nil))
      (funcall f arg)))

  (advice-add 'undo-tree-undo :around #'undo-keep-region)

  (global-undo-tree-mode)

  :bind
  ("s-z" . undo-tree-undo)
  ("s-Z" . undo-tree-redo)
  ("C-s-z" . undo-tree-visualize))

(use-package goto-chg
  :bind
  ("C-." . goto-last-change)
  ("C-s-." . goto-last-change-reverse))

;; TODO Make this work.  Also the code does not look high quality.
;; (use-package gumshoe
;;   :defer 11
;;   :straight (gumshoe :type git
;;                      :host github
;;                      :repo "Overdr0ne/gumshoe"
;;                      :branch "master")

;;   :preface
;;   (defun consult-gumshoe ()
;;     (interactive)
;;     (consult-global-mark (ring-elements gumshoe--log)))

;;   :config
;;   ;; The minor mode must be enabled to begin tracking
;;   (global-gumshoe-mode 1)

;;   :bind
;;   ("M-g C-g" . consult-gumshoe))

;; (use-package easy-kill
;;   :bind
;;   (([remap kill-ring-save] . easy-kill)
;;    ([remap mark-sexp] . easy-mark)))

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
  :bind
  ("M-<down-mouse-1>" . mc/toggle-cursor-on-click)
  ("C-S-c C-S-c" . mc/edit-lines)
  ("M-s-m" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/unmark-next-like-this)
  ("C-M-<" . mc/mark-previous-like-this)
  ("C-M->" . mc/unmark-previous-like-this)
  ("C-c >" . mc/mark-all-dwim)
  ("C-c C-a"  . mc/mark-all-dwim)
  (:map mc/keymap
        ("C-'" . mc-hide-unmatched-lines-mode)))

(use-package move-text
  :bind
  (:map prog-mode-map
        ("M-<up>" . move-text-up)
        ("M-<down>" . move-text-down)))

(use-package string-inflection
  :config
  (defalias #'string-inflection-snakecase #'string-inflection-underscore)
  :bind
  ("C-c C-i" . string-inflection-all-cycle))

(use-package yasnippet
  :custom
  ;; Don't write messages at startup.
  (yas-verbosity 1)
  :config
  (yas-global-mode)
  :bind
  ("s-'" . yas-expand)
  ("C-c C-y" . yas-insert-snippet))

(use-package yasnippet-snippets
  :after yasnippet
  :config
  (yasnippet-snippets-initialize))

(use-package smartparens
  :defer 3

  :custom
  ;; Don't kill the entire symbol with `sp-kill-hybrid-sexp'. If we want to kill
  ;; the entire symbol, use `sp-kill-symbol'.
  (sp-hybrid-kill-entire-symbol nil)
  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (sp-cancel-autoskip-on-backward-movement nil)

  :functions
  sp--get-opening-regexp
  sp--get-closing-regexp
  sp-local-pair
  sp-get-pair
  sp-with-modes
  sp-point-in-string-or-comment

  :commands
  sp-forward-slurp-sexp
  sp-backward-symbol
  sp-backward-symbol
  sp-down-sexp
  sp-forward-sexp
  sp-backward-sexp

  :config
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

  (defvar sh-basic-offset)

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

  ;; (declare-function thing-at-point-looking-at 'thingatpt)

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
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (require 'smartparens-config)

  (sp-with-modes '(c-mode c++-mode caddyfile-mode csharp-mode css-mode
                          graphql-mode java-mode javascript-mode js-mode
                          js2-mode json-mode lua-mode objc-mode swift-mode
                          web-mode)
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

  ;; The scratch buffer loads before smartparens.
  (with-current-buffer "*scratch*" (turn-on-smartparens-mode))

  (eldoc-add-command #'sp-newline)

  :hook
  (smartparens-mode-hook . show-smartparens-mode)
  ((cider-repl-mode-hook conf-mode-hook prog-mode-hook text-mode-hook
                         toml-mode-hook minibuffer-setup-hook)
   . smartparens-mode)

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
        ("C-M-S-u" . sp-up-sexp)
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
        ("M-{" . sp-wrap-curly)
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

(use-package whole-line-or-region
  :bind
  ([remap kill-region] . whole-line-or-region-kill-region)
  ([remap kill-ring-save] . whole-line-or-region-kill-ring-save)
  ([remap copy-region-as-kill] . whole-line-or-region-copy-region-as-kill)
  ([remap ns-copy-including-secondary] . whole-line-or-region-copy-region-as-kill)
  ([remap delete-region] . whole-line-or-region-delete-region)
  ([remap comment-dwim] . whole-line-or-region-comment-dwim-2)
  ([remap comment-region] . whole-line-or-region-comment-region)
  ([remap uncomment-region] . whole-line-or-region-uncomment-region))

(use-package evil-nerd-commenter
  :bind
  ("M-;" . evilnc-comment-or-uncomment-lines)
  ("s-/" . evilnc-comment-or-uncomment-lines)
  ("C-M-;" . evilnc-comment-or-uncomment-paragraphs)
  ("C-s-;" . evilnc-quick-comment-or-uncomment-to-the-line))

(use-package ws-butler
  :hook
  (prog-mode-hook . ws-butler-mode))

(defun clipboard-yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (if (and delete-selection-mode (use-region-p)) (delete-active-region))
  (let ((start (point)))
    (clipboard-yank)
    (indent-region start (point))))

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (beginning-of-line)
  (set-mark (line-end-position)))

(bind-keys
 ("s-v" . clipboard-yank-and-indent)
 ("C-M-}" . forward-sentence)
 ("C-M-{" . backward-sentence)
 ("C-o" . open-line-above)
 ("C-^" . delete-indentation-forward)
 ("s-C" . copy-line-or-region-to-other-window)
 ("s-X" . move-line-or-region-to-other-window)
 ("s-l" . select-current-line)
 ;; Replace `delete-horizontal-space' with the more useful `cycle-spacing'.
 ("M-\\" . cycle-spacing)
 ;; Continue comment on next line (default binding is "C-M-j")
 ("M-RET" . indent-new-comment-line)
 ("M-z" . zap-to-char)
 ("M-Z" . zap-up-to-char))


;;;; Shell

;; Shell, Term, Tramp, Scripting, and related things.

(use-package comint
  :straight (:type built-in)
  :custom
  (comint-buffer-maximum-size 20000)
  (comint-prompt-read-only t)
  (comint-terminfo-terminal "eterm-256color")
  :config
  (defun comint-setup ()
    "Set up `comint-mode'."
    ;; Remove the echo of input after pressing RET in `shell-mode'. Not sure why
    ;; this is needed. Is it a zsh thing?
    (setq comint-process-echoes t))
  :hook
  (comint-mode-hook . comint-setup))

(use-package ssh-agency
  :defer 5
  :config
  (defun ssh-agency-list-keys ()
    "List the currently loaded ssh-agent keys."
    (interactive)
    (message
     (string-trim-right
      (let ((default-directory user-emacs-directory))
        (shell-command-to-string (concat ssh-agency-add-executable " -l"))))))

  (defun ssh-agency-ensure-without-keys ()
    "Start ssh-agent but don't add keys.

Intended to be used at Emacs startup so we run ssh-agent but
don't immediately bother the user with adding keys.  Keys will be
added as they are used."
    (message "Starting ssh-agent...")
    (or (ssh-agency-status)
        (ssh-agency-find-agent)
        (ssh-agency-start-agent)))

  (ssh-agency-ensure-without-keys))

(use-package ssh
  :custom
  (ssh-directory-tracking-mode 'ftp)

  :commands
  ssh-host

  :config
  (defun ssh-host ()
    "Like `ssh' only choose from a list of known hosts.

If prefix arg is non-nil, read ssh arguments from the minibuffer."
    (interactive)
    (let ((default-directory "~"))
      (ssh (concat (when prefix-arg
                     (concat (read-from-minibuffer "ssh arguments: ") " "))
                   (ssh-choose-host)))))

  (defun ssh-mode-setup ()
    "Set up `ssh-mode'."
    (shell-dirtrack-mode t)
    (setq dirtrackp nil))

  :hook
  (ssh-mode-hook . ssh-mode-setup))

(use-package ssh-config-mode
  :mode
  ("/\\.?sshd?/config\\'" . ssh-config-mode)
  ("/known_hosts\\'" . ssh-known-hosts-mode)
  ("/authorized_keys2?\\'" . ssh-authorized-keys-mode))

;; TRAMP is updated more regularly than Emacs, so pull it from ELPA.
(use-package tramp
  ;; FIXME The git version of TRAMP currently doesn't seem to work with straight.
  ;; :straight (:type built-in)
  :functions
  tramp-cleanup-all
  tramp-insert-remote-part
  tramp-dired
  :config
  (defun tramp-cleanup-all ()
    "Clean up all tramp buffers and connections."
    (interactive)
    (tramp-cleanup-all-buffers)
    (tramp-cleanup-all-connections))

  (defun tramp-insert-remote-part ()
    "Insert current tramp prefix at point."
    (interactive)
    (if-let* ((remote (file-remote-p default-directory)))
        (insert remote)))

  (defun tramp-dired (host)
    "Choose an ssh HOST and then open it with dired."
    (interactive (list (ssh-choose-host "Hostname or tramp path: ")))
    (find-file
     (if (tramp-file-name-p host)
         host
       (find-file (concat "/ssh:" host ":")))))

  ;; Configure TRAMP to respect the PATH variable on the remote machine (for
  ;; remote eshell sessions)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  :bind
  ("C-c d" . tramp-dired)
  ("C-:" . tramp-insert-remote-part))

(defun insert-environment-variable ()
  "Insert contents of an envionment variable at point."
  (interactive)
  (insert (getenv (read-envvar-name "Insert Environment Variable: "))))

(defun insert-variable-value ()
  "Insert the value of Emacs Lisp variable at point."
  (interactive)
  (insert (prin1-to-string
           (symbol-value (read-variable "Insert Emacs Lisp Variable Value: ")))))

(defun list-hosts-from-known-hosts ()
  "Return a list of hosts from `~/.ssh/known_hosts'."
  (with-temp-buffer
    (insert-file-contents-literally "~/.ssh/known_hosts")
    (cl-remove-if (lambda (host) (string= "" host))
                  (mapcar (lambda (line) (replace-regexp-in-string
                                          "\\]\\|\\[" ""
                                          (car (split-string line "[, :]"))))
                          (split-string (buffer-string) "\n")))))

(defun list-hosts-from-ssh-config ()
  "Return a list of hosts from `~/.ssh/config'."
  (with-temp-buffer
    (insert-file-contents-literally "~/.ssh/config")
    (goto-char (point-min))
    (let (hosts)
      (while (re-search-forward "^Host " nil t)
        (while (re-search-forward "\\_<[0-9a-zA-Z._-]+\\_>" (line-end-position) t)
          (push (buffer-substring (match-beginning 0) (match-end 0)) hosts)))
      hosts)))

(defun list-hosts-from-etc-hosts ()
  "Return a list of hosts from `/etc/hosts'."
  (with-temp-buffer
    (insert-file-contents-literally "/etc/hosts")
    (flush-lines "^#")
    (flush-lines "^$")
    (cl-remove-if (lambda (host) (or (string= host "localhost")
                                     (string= host "broadcasthost")
                                     (eq host nil)))
                  (mapcar (lambda (line) (cadr (split-string line "[ \t]+")))
                          (split-string (buffer-string) "\n")))))

(defvar recentf-list)

(defun list-hosts-from-recentf ()
  "Return a list of hosts from the `recentf-list'."
  (cl-remove-duplicates
   (let (hosts)
     (dolist (host recentf-list)
       (when (string-match "^/\\(?:s\\(?:cp\\|shx?\\)\\)\?:\\([^:|]+\\)[:|]" host)
         (push (match-string-no-properties 1 host) hosts)))
     hosts)
   :test #'string=))

(defun list-hosts ()
  "Return a list of remote hosts from various sources."
  (cl-remove-duplicates
   (append
    (list-hosts-from-recentf)
    (list-hosts-from-known-hosts)
    (list-hosts-from-ssh-config)
    (list-hosts-from-etc-hosts))
   :test #'string=))

(defvar ssh-choose-host-history nil
  "History for the `ssh-choose-host' command.")

(defun ssh-choose-host (&optional prompt)
  "Make a list of recent ssh hosts and interactively choose one with optional PROMPT."
  (completing-read (or prompt "SSH to Host: ") (list-hosts) nil nil nil ssh-choose-host-history))

(use-package shell
  :config
  (defun comint-delchar-or-eof-or-kill-buffer (arg)
    "`C-d' on an empty line in the shell terminates the process, accepts ARG.

 Stolen from http://whattheemacsd.com/setup-shell.el-01.html."
    (interactive "p")
    (if (null (get-buffer-process (current-buffer)))
        (kill-buffer)
      (comint-delchar-or-maybe-eof arg)))

  (defun shell-rename-buffer (_)
    "Rename buffer to `default-directory'."
    (rename-buffer (format "*shell* (%s)" (shorten-file-name default-directory)) t))

  (defun async-shell-command-run-last ()
    "Run the last shell command asynchronously."
    (interactive)
    (async-shell-command (car shell-command-history)))

  (defun shell-mode-setup ()
    "Set up `shell-mode'."
    (setenv "TERM" "xterm-256color"))

  :hook
  (shell-mode-hook . shell-dirtrack-mode)
  (shell-mode-hook . shell-mode-setup)
  ;; Disabled because it turns out multiple things assume `shell-mode' buffers
  ;; have their default naming conventions.
  ;; (comint-output-filter-functions . shell-rename-buffer)

  :bind
  ("C-S-s" . async-shell-command-run-last)
  (:map shell-mode-map
        ("C-d" . comint-delchar-or-eof-or-kill-buffer)
        ("SPC" . comint-magic-space)
        ("M-r" . counsel-shell-history)))

(use-package native-complete
  :after shell
  :config
  (native-complete-setup-bash))

(use-package with-editor
  :commands
  crontab
  :config
  (defun crontab ()
    "Run `crontab -e' in an Emacs buffer.

Stolen from https://emacs.stackexchange.com/questions/10077/how-to-edit-crontab-directly-within-emacs-when-i-already-have-emacs-open."
    (interactive)
    (with-editor-async-shell-command "crontab -e")))

(use-package crontab-mode
  :mode "/crontab\\(\\.X*[[:alnum:]]+\\)?\\'"
  :init
  (with-eval-after-load 'mixed-pitch-mode
    (add-to-list 'mixed-pitch-exclude-modes 'crontab-mode)))

(use-package compile
  :custom
  (compilation-scroll-output t)
  (compilation-ask-about-save nil)
  (compile-command "")
  ;; Don't prompt to save files before compiling.
  (compilation-save-buffers-predicate (lambda () nil))
  :bind
  ("C-x c" . compile))

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
        (concat "/" hop method ":"
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

(declare-function 'eshell-return-to-prompt "em-hist")
(declare-function 'eshell-send-input "esh-mode")

(defun sudo-toggle ()
  "Reopen the current buffer as root.

Works on file, `dired', `shell', `eshell', and `vterm' buffers.

A sudo enabled buffer is opened and the non-sudo buffer is left
intact.  `vterm' is an exception--for that we just type 'sudo -i'
into the terminal and hope for the best."
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
           (cd newf)
           (eshell-emit-prompt))
          ((derived-mode-p 'vterm-mode)
           (vterm-send-C-a)
           (vterm-send-C-k)
           (vterm-insert "sudo -i")
           (vterm-send-return))
          (t (message "Can't sudo this buffer")))))

(defun filter-functions (regexp &optional predicate)
  "Return a list of functions whose names match REGEXP.

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

(use-package term
  :bind
  (:map term-mode-map
        ("M-p" . term-send-up)
        ("M-n" . term-send-down))
  (:map term-raw-map
        ("M-o" . other-window)
        ("M-p" . term-send-up)
        ("M-n" . term-send-down)
        ("C-M-j" . term-switch-to-shell-mode)))

(use-package vterm
  :init
  (add-to-list 'hl-line-disabled-mode-list 'vterm-mode)

  :custom
  (vterm-always-compile-module t)
  (vterm-buffer-name-string "*VTerm %s*")
  (vterm-kill-buffer-on-exit nil)

  :config
  ;; TODO Make these functions using macro?  And eshell analogs.
  (defun vterm-create-in-background ()
    "Create a new VTerm buffer but don't display it."
    (save-window-excursion (vterm "*VTerm*")))

  (defun vterm-get-or-create ()
    "Get or create an Eshell buffer."
    (interactive)
    (or (when current-prefix-arg (vterm-create-in-background))
        (car (filter-buffers-by-mode 'vterm-mode))
        (vterm-create-in-background)))

  (defun vterm-switch-to-buffer ()
    "Switch to the most recent VTerm buffer or create a new one."
    (interactive)
    (switch-to-buffer (vterm-get-or-create)))

  (defun vterm-switch-to-buffer-other-window ()
    "Get or create a VTerm buffer, then switch to it."
    (interactive)
    (switch-to-buffer-other-window (vterm-get-or-create)))

  (defun vterm-choose-buffer ()
    "Interactively choose a VTerm buffer."
    (interactive)
    (counsel-switch-buffer-by-mode 'vterm-mode))

  :bind
  ("s-t" . vterm-switch-to-buffer)
  ("C-c t" . vterm-switch-to-buffer)
  ("s-T" . vterm-switch-to-buffer-other-window)
  ("C-c C-t" . vterm-switch-to-buffer-other-window)
  (:map vterm-mode-map
        ;; Override the normal `clipboard-yank-and-indent'.
        ("s-v" . vterm-yank)
        ;; Provide consistency with `eshell', `shell-mode'.
        ("M-p" . vterm-send-up)
        ("M-n" . vterm-send-down)
        ;; macOS binding for undo.
        ("s-z" . vterm-undo)))

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

  (with-eval-after-load 'esh-mode
    (defvar eshell-preoutput-filter-functions)
    (defvar eshell-output-filter-functions)
    (add-to-list 'eshell-preoutput-filter-functions #'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

  (with-eval-after-load 'compile
    (defvar compilation-environment)
    (add-to-list 'compilation-environment "TERM=xterm-256color")
    (defun xterm-color-compilation-filter (f proc string)
      (funcall f proc (xterm-color-filter string)))

    ;; Emacs 28 renamed `compilation-filter' to `compilation-filter-hook'.
    (advice-add (if (boundp 'compilation-filter-hook)
                    'compilation-filter-hook
                  'compilation-filter) :around #'xterm-color-compilation-filter))

  (setenv "TERM" "eterm-256color")

  :hook
  (shell-mode-hook . xterm-color-shell-setup))

(use-package piper
  :straight (piper :host gitlab :repo "howardabrams/emacs-piper")
  :init
  (bind-prefix piper-map "C-c |" "Piper commands")
  :bind
  ("C-c | |" . piper)
  ("C-c | o" . piper-other)
  ("C-c | r" . piper-remote))

(defun copy-buffer-file-name ()
  "Copy variable `buffer-file-name' to the kill ring."
  (interactive)
  (kill-new buffer-file-name)
  (message buffer-file-name))

(defgroup fpw nil
  "Generate random passwords securely?"
  :group 'external
  :link '(url-link :tag "GitHub" "https://github.com/mnewt/fpw"))

(defcustom fpw-args "-LUDSn 10"
  "Arguments for the `fpw' command."
  :group 'fpw
  :type 'string)

(defun fpw (command)
  "Run `fpw' command as COMMAND.

Copy the result to the `kill-ring'.  Call with a prefix argument
to modify the args.

Create a buffer named *Fun Password Generator* but only pop it
open when a prefix arg is specified.

See https://github.com/mnewt/fpw."
  (interactive
   (list (let ((command (concat "fpw " fpw-args)))
           (if current-prefix-arg
               (read-shell-command "Run fpw (like this): "
                                   command
                                   'fpw-history)
             command))))
  (let ((buffer (get-buffer-create "*Fun Password Generator*")))
    (shell-command command buffer)
    (with-current-buffer buffer
      (save-excursion
        (forward-line -1)
        (copy-region-as-kill (line-beginning-position) (line-end-position))))
    (message "Copied to kill-ring: %s" (car kill-ring))
    (when current-prefix-arg
      (pop-to-buffer buffer))))

(bind-keys
 ("C-c v" . insert-variable-value)
 ("C-c C-v" . insert-environment-variable)
 :map m-map
 ("p" . fpw)
 :map m-file-map
 ("c" . copy-buffer-file-name)
 :map m-toggle-map
 ("s" . sudo-toggle))


;;;; Eshell

(use-package eshell
  :defer 29
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
  (eshell-ls-clutter-regexp
   (regexp-opt '(".cache" ".DS_Store" ".Trash" ".lock" "_history" "-history"
                 ".tmp" "~" "desktop.ini" "Icon\r" "Thumbs.db" "$RECYCLE_BIN"
                 "lost+found")))
  (eshell-history-size 10000)

  :commands
  eshell
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
    (or (unless current-prefix-arg
          (car (filter-buffers-by-mode 'eshell-mode)))
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

  (defun eshell-choose-buffer ()
    "Interactively choose an Eshell buffer."
    (interactive)
    (counsel-switch-buffer-by-mode 'eshell-mode))

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

  (defface eshell-prompt-sigil '((default :weight bold)
                                 (((background  dark)) :foreground "white")
                                 (((background light)) :foreground "black"))
    "Face for the prompt sigil."
    :group 'eshell)

  (defface eshell-prompt-error
    '((t :background "red" :foreground "white" :weight bold))
    "Face for prompt errors."
    :group 'eshell)

  (defface eshell-prompt-directory '((t :background "cyan" :foreground "black"))
    "Face for the directory in the prompt."
    :group 'eshell)

  (defun m-eshell-prompt-function ()
    "Produce a highlighted prompt for Eshell."
    (mapconcat
     (lambda (el)
       (when el
         (propertize (concat " " (car el) " ")
                     'read-only t
                     'font-lock-face (cdr el)
                     'front-sticky '(font-lock-face read-only)
                     'rear-nonsticky '(font-lock-face read-only))))
     `(,(unless (eshell-exit-success-p)
          `(,(number-to-string eshell-last-command-status) eshell-prompt-error))
       (,(abbreviate-file-name (eshell/pwd)) eshell-prompt-directory)
       (,(if (or (zerop (user-uid)) (string-match-p "sudo:" default-directory))
             "\n(#)" "\n()")
        eshell-prompt-sigil))
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
 allowed.

Stolen from https://gist.github.com/ralt/a36288cd748ce185b26237e6b85b27bb."
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
    ;; Path to shell executable. Set it this way to work with tramp.
    ;; (setenv "SHELL" "/bin/bash")
    ;; (setenv "TERM" "eterm-color")
    (setenv "EDITOR" "emacsclient")
    (setenv "PAGER" "cat")
    (setenv "MANPAGER" "cat")

    (defvar eshell-visual-commands)
    (add-to-list 'eshell-visual-commands "n")
    (add-to-list 'eshell-visual-commands "htop")
    (add-to-list 'eshell-visual-commands "glances")

    ;; Load the Eshell versions of `su' and `sudo'
    (require 'em-tramp)
    (add-to-list 'eshell-modules-list 'eshell-tramp)

    ;; Set up `tramp-colon-prefix'.
    (add-hook 'post-self-insert-hook #'tramp-colon-prefix-maybe-expand nil t)

    (defvar eshell-mode-map)
    (bind-keys
     :map eshell-mode-map
     ("C-a" . eshell-maybe-bol)
     ("C-d" . eshell-quit-or-delete-char)
     ("<tab>" . completion-at-point)
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

  (defun eshell-ls-file-at-point ()
    "Get the full path of the Eshell listing at point."
    (expand-file-name (substring-no-properties (thing-at-point 'filename))
                      (get-text-property (point) 'eshell-ls-path)))

  (defun eshell-ls-find-file ()
    "Open the Eshell listing at point."
    (interactive)
    (find-file (eshell-ls-file-at-point)))

  (defun eshell-ls-delete-file ()
    "Delete the Eshell listing at point."
    (interactive)
    (let ((file (eshell-ls-file-at-point)))
      (when (yes-or-no-p (format "Delete file %s?" file))
        (delete-file file 'trash))))

  (defvar eshell-ls-file-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "RET") #'eshell-ls-find-file)
      (define-key map (kbd "<return>") #'eshell-ls-find-file)
      (define-key map [mouse-1] #'eshell-ls-find-file)
      (define-key map (kbd "D") #'eshell-ls-delete-file)
      map)
    "Keys in effect when point is over a file from `eshell/ls'.")

  (defun eshell-ls-decorated-name-improved (f file)
    "Add features to listings in `eshell/ls' output.

The features are:
1. Add decoration like 'ls -F':
 * Mark directories with a `/'
 * Mark execurables with a `*'

2. Make each listing into a clickable link to open the
corresponding file or directory.

This function is meant to be used as advice around
`eshell-ls-decorated-name', where F is `eshell-ls-decorated-name'
and FILE is the cons describing the file."
    (let* ((name (funcall f file))
           (suffix
            (cond
             ;; Directory
             ((eq (cadr file) t)
              "/")
             ;; Executable
             ((and (/= (user-uid) 0) ; root can execute anything
                   (eshell-ls-applicable (cdr file) 3 #'file-executable-p (car file)))
              "*"))))
      (concat (propertize name
                          'keymap eshell-ls-file-keymap
                          'mouse-face 'highlight
                          'eshell-ls-path default-directory)
              (when (and suffix (not (string-suffix-p suffix name)))
                (propertize suffix 'face 'shadow)))))

  (advice-add 'eshell-ls-decorated-name :around #'eshell-ls-decorated-name-improved)

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
  ("s-e" . eshell-switch-to-buffer)
  ("C-c e" . eshell-switch-to-buffer)
  ("s-E" . eshell-switch-to-buffer-other-window)
  ("C-c E" . eshell-switch-to-buffer-other-window)
  ("C-s-e" . eshell-choose-buffer)
  ("M-E" . ibuffer-show-eshell-buffers)
  ("C-c M-e" . ibuffer-show-eshell-buffers))

;; ElDoc in Eshell.
(use-package esh-help
  :after eshell

  :config
  (defun esh-help-eldoc-command-tramp-short-circuit (f)
    "Short circuit `esh-help' from TRAMP buffers."
    (unless (file-remote-p default-directory)
      (funcall f)))

  (advice-add 'esh-help-eldoc-command
              :around #'esh-help-eldoc-command-tramp-short-circuit)

  (defun esh-help-setup ()
    "Setup eldoc function for Eshell."
    (setq-local eldoc-documentation-function #'esh-help-eldoc-command))
  :hook
  (eshell-mode-hook . esh-help-setup))

;; Fish-like autosuggestions.
(use-package esh-autosuggest
  :after eshell
  :config
  (defvar esh-autosuggest-active-map)
  (defun esh-autosuggest-setup ()
    "Set up `esh-autosuggest-mode'."
    (esh-autosuggest-mode)
    (bind-key "C-e" #'company-complete-selection esh-autosuggest-active-map))
  :hook
  (eshell-mode-hook . esh-autosuggest-setup))

(use-package eshell-bookmark
  :after eshell
  :hook
  (eshell-mode-hook . eshell-bookmark-setup))

(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))


;;;; Lisp

;; Lisp specific functionality

(use-package elisp-mode
  :straight (:type built-in)
  :defer 19
  :mode ("Cask\\'" . emacs-lisp-mode)
  :preface
  (defun emacs-lisp-mode-setup ()
    "Set up `emacs-lisp-mode'."
    ;; Ugh. It's the Emacs Lisp standard.
    (setq-local sentence-end-double-space t))

  :config
  (add-to-list 'safe-local-variable-values
               '(flycheck-checkers . (emacs-lisp emacs-lisp-checkdoc)))

  :hook
  (emacs-lisp-mode-hook . emacs-lisp-mode-setup)

  :bind
  ;; `lisp-mode-shared-map' is shared between `emacs-lisp-mode' and
  ;; `lisp-interaction-mode'.
  (:map lisp-mode-shared-map
        ("s-<return>" . eval-last-sexp)
        ("s-RET" . eval-last-sexp)
        ("C-s-<return>" . eval-last-sexp-other-window)
        ("C-c C-k" . eval-buffer)
        ("C-x C-r" . eval-region)
        ("C-x M-p" . pp-eval-last-sexp)
        ("C-x M-e" . pp-macroexpand-last-sexp)
        ("C-x r E" . expression-to-register)
        ("C-x r e" . eval-register)
        ("M-s-t" . ert-run-test-at-point)
        ("C-s-t" . ert-reload-and-run-tests-in-project)))

(use-package lisp-extra-font-lock
  :hook
  (lisp-data-mode-hook . lisp-extra-font-lock-mode))

(use-package profiler
  :preface
  (defun profiler-dwim (and-mem)
    "Toggle `profiler'.

If `profiler' is stopped, start it in cpu mode.

If `profiler' is started, stop it and run `profiler-report'.

If AND-MEM is non-nil, profile memory as well."
    (interactive "P")
    (if (not (and (fboundp 'profiler-cpu-running-p) (profiler-cpu-running-p)))
        (profiler-start (if and-mem 'cpu+mem 'cpu))
      (profiler-stop)
      (profiler-report)))

  :bind
  ("M-m M-p" . profiler-dwim))

(use-package re-builder
  :custom
  ;; string syntax means you don't need to double escape things.
  (reb-re-syntax 'read)

  :config
  (defun reb-query-replace (to-string)
    "Replace current RE from point with `query-replace-regexp'."
    (interactive
     (progn (barf-if-buffer-read-only)
            (list (query-replace-read-to (reb-target-binding reb-regexp)
                                         "Query replace"  t))))
    (with-current-buffer reb-target-buffer
      (query-replace-regexp (reb-target-binding reb-regexp) to-string)))

  (defun reb-beginning-of-buffer ()
    "In re-builder, move target buffer point position back to beginning."
    (interactive)
    (set-window-point (get-buffer-window reb-target-buffer)
                      (with-current-buffer reb-target-buffer (point-min))))

  (defun reb-end-of-buffer ()
    "In re-builder, move target buffer point position back to beginning."
    (interactive)
    (set-window-point (get-buffer-window reb-target-buffer)
                      (with-current-buffer reb-target-buffer (point-max))))

  :bind
  (:map prog-mode-map
        ("C-c x" . re-builder)))

(use-package pcre2el
  :hook
  ((emacs-lisp-mode-hook lisp-interaction-mode-hook reb-mode-hook) . rxt-mode))

(use-package parinfer
  :custom
  (parinfer-extensions
   '(defaults        ; should be included.
      pretty-parens  ; different paren styles for different modes.
      smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
      smart-yank))   ; Yank behavior depends on mode.

  :config
  (parinfer-strategy-add 'default 'newline-and-indent)
  (parinfer-strategy-add 'instantly
    '(parinfer-smart-tab:dwim-right
      parinfer-smart-tab:dwim-right-or-complete
      parinfer-smart-tab:dwim-left))
  (setq parinfer-lighters '("➠" ")"))

  :hook
  ((clojure-mode-hook
    emacs-lisp-mode-hook
    hy-mode-hook
    lisp-interaction-mode-hook
    lisp-mode-hook
    scheme-mode-hook) . parinfer-mode)
  :bind
  (:map parinfer-mode-map
        ("<tab>" . parinfer-smart-tab:dwim-right-or-complete)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)
        ("C-," . parinfer-toggle-mode)
        ;; Don't interfere with smartparens quote handling
        ("\"" . nil)
        ("RET" . sp-newline)
        ("<return>" . sp-newline))
  (:map parinfer-region-mode-map
        ("C-i" . indent-for-tab-command)
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)))

(defun advice-functions-on-symbol (symbol)
  "Return a list of functions advising SYMBOL."
  (let (advices)
    (advice-mapc (lambda (advice _props) (push advice advices))
                 symbol)
    advices))

(defun advice-symbols ()
  "Return a list of symbols which have been advised."
  (let (symbols)
    (mapatoms
     (lambda (symbol)
       (when-let ((advices (advice-functions-on-symbol symbol)))
         (push (cons symbol advices) symbols))))
    symbols))

(defun advice-remove-interactively (symbol function)
  "Interactively remove FUNCTION advice on SYMBOL."
  (interactive
   (let* ((symbols (advice-symbols))
          (symbol (intern (completing-read "Remove advice from symbol: "
                                           (mapcar #'car symbols))))
          (function (intern (completing-read
                             (format "Remove function advice from %s: " symbol)
                             (assoc-default symbol symbols)))))
     (list symbol function)))
  (advice-remove symbol function)
  (message "Removed advice from %s: %s." symbol function))

(defun advice-remove-all (symbol)
  "Remove all advices from SYMBOL."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove symbol advice)) symbol))

(defun hook-symbols ()
  "Return a list symbols which are likely to be hooks variables."
  (let (symbols)
    (mapatoms
     (lambda (symbol)
       (let ((name (symbol-name symbol)))
         (when (and (or (string-suffix-p "-hook" name)
                        (string-suffix-p "-functions" name))
                    (boundp symbol)
                    (consp (symbol-value symbol)))
           (push symbol symbols)))))
    symbols))

;; TODO Look at point for an `add-hook' expression to pre-populate
;; `completing-read'.
(defun remove-hook-interactively (hook function)
  "Interactively remove FUNCTION from HOOK."
  (interactive
   (let* ((hooks (hook-symbols))
          (hook (intern (completing-read "Remove function from hook: "
                                         hooks)))
          (function (intern (completing-read
                             (format "Remove function from hook %s: " hook)
                             (symbol-value hook)))))
     (list hook function)))
  (remove-hook hook function)
  (message "Removed %s from hook %s." function hook))

(defun unintern-prefix (prefix)
  "Unintern all symbols starting with PREFIX."
  (interactive
   (list (completing-read
          "Prefix: " features nil nil
          (file-name-nondirectory (file-name-sans-extension buffer-file-name)))))
  (mapatoms (lambda (symbol)
              (if (string-prefix-p prefix (symbol-name symbol))
                  (unintern symbol nil)))))

(defun elisp-test-file-p (file)
  "Check whether the FILE name is an Elisp test file."
  (or (string-suffix-p "-test.el" file)
      (string-match-p "test/" file)))

(defun ert-run-test-at-point ()
  "Run the test at point."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (forward-symbol 2)
    (ert-run-tests-interactively
     (substring-no-properties (symbol-name (symbol-at-point))))))

(declare-function ert-delete-all-tests "ert")

(defun ert-reload-and-run-tests-in-project (&optional reset)
  "Reload project and test files and then run deftests.

When RESET is non-nil, unintern all the functions with the
project prefix, and unintern all `ert' tests."
  (interactive "P")
  (require 'ert)
  (require 'projectile)
  (let* ((default-directory (projectile-project-root))
         (all-el-files (split-string (shell-command-to-string "find . -name '*.el'")))
         (el-files (cl-remove-if #'elisp-test-file-p all-el-files))
         (test-files (cl-remove-if-not #'elisp-test-file-p all-el-files))
         (prefix (file-name-nondirectory (directory-file-name default-directory))))
    (when reset
      ;; Delete all functions with the project prefix
      (mapatoms (lambda (symbol)
                  (and (functionp symbol)
                       (string-prefix-p prefix (symbol-name symbol))
                       (fmakunbound symbol))))
      ;; Delete all tests.
      (ert-delete-all-tests))
    ;; Reload the project namespace.
    (dolist (file el-files) (load file nil nil nil t))
    ;; Reload tests.
    (dolist (file test-files) (load file nil nil nil t))
    ;; Run tests.
    (ert-run-tests-interactively prefix)))

(define-minor-mode ert-run-on-save-mode
  "Minor mode to run `ert' tests on save."
  :init-value nil
  :group 'ert
  (if ert-run-on-save-mode
      (add-hook 'after-save-hook #'ert-reload-and-run-tests-in-project nil 'local)
    (remove-hook 'after-save-hook #'ert-reload-and-run-tests-in-project 'local)))

(defun eval-last-sexp-other-window (arg)
  "Run `eval-last-sexp' with ARG in the other window."
  (interactive "P")
  (save-window-excursion (other-window 1)
                         (eval-last-sexp arg)))

(defun expression-to-register (register)
  "Interactively store an Emacs Lisp expression in a REGISTER.
If region is active, store that.  Otherwise, store the sexp at
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
  (interactive (list (register-read-with-preview "Eval register: ")))
  (let* ((val (get-register register))
         (res (eval (car (read-from-string val)))))
    (if current-prefix-arg
        (register-val-insert res)
      (pp res))))

(use-package debbugs
  :commands
  debbugs-gnu
  debbugs-gnu-search
  debbugs-gnu-bugs)

(use-package clojure-mode
  :mode
  ("\\.clj\\'" . clojure-mode)
  ("\\.cljs\\'" . clojurescript-mode)
  ("\\.cljc\\'" . clojurec-mode)
  ("\\.edn\\'" . clojure-mode)

  :interpreter
  ("inlein" . clojure-mode)

  :hook
  (clojure-mode-hook . subword-mode))

(use-package clojure-mode-extra-font-locking
  :after clojure)

(use-package clj-refactor
  :after clojure
  :config
  (defun clj-refactor-setup ()
    "Set up `clj-refactor-mode'."
    (clj-refactor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  :hook
  (clojure-mode-hook . clj-refactor-setup)
  :bind
  (:map clojure-mode-map
        ("C-s-r" . cljr-rename-symbol)))

;; > brew install borkdude/brew/clj-kondo
(use-package flycheck-clj-kondo
  :after clojure)

(use-package inf-clojure
  :after clojure
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

  (defvar inf-clojure-minor-mode-map)
  (bind-keys :map inf-clojure-minor-mode-map
             ("s-<return>" . inf-clojure-eval-last-sexp)
             ("C-c C-k" . inf-clojure-eval-buffer)))

(use-package cider
  :after clojure
  :custom
  ;; Never prompt when looking up a symbol.
  (cider-prompt-for-symbol nil)
  ;; Always prompt for the jack in command.
  (cider-edit-jack-in-command t)

  :functions
  cider--find-var
  cider--find-var-other-window

  :config
  (defun cider-find-var-other-window (&optional arg _var _line)
    "Find the var in the other window."
    (interactive "P")
    (funcall (cider-prompt-for-symbol-function arg)
             "Symbol"
             (if (cider--open-other-window-p arg)
                 #'cider--find-var
               #'cider--find-var-other-window)))

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

  (defvar cider-stacktrace-frames-background-color)

  (defun cider-stacktrace-adapt-to-theme (&rest _)
    "The standard advice function runs at the wrong time I guess?
  Anyway, it often gets set to the wrong color when switching
  themes via `theme-choose'."
    (when (fboundp 'cider-scale-background-color)
      (setq cider-stacktrace-frames-background-color
            (cider-scale-background-color))))

  (advice-add #'load-theme :after #'cider-stacktrace-adapt-to-theme)

  (defun cider-pprint-register (register)
    "Eval REGISTER as clojure code and pretty print the result.

https://lambdaisland.com/blog/2019-12-20-advent-of-parens-20-life-hacks-emacs-ginger-tea."
    (interactive (list (register-read-with-preview "Eval register: ")))
    (cider--pprint-eval-form (get-register register)))

  :hook
  (cider-mode-hook . cider-company-enable-fuzzy-completion)
  (cider-repl-mode-hook . cider-company-enable-fuzzy-completion)

  :bind
  (:map cider-mode-map
        ("s-<return>" . cider-eval-last-sexp)
        ("C-x 4 ." . cider-find-var-other-window)))

(use-package sly
  :mode
  ("\\.lisp\\'" . sly-mode)
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


;;;; Reading

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-loader-install)
  :bind
  (:map pdf-view-mode-map
        ("s-f" . isearch-forward)))

(use-package pdf-continuous-scroll-mode
  :after pdf-tools
  :straight (:host github :repo "dalanicolai/pdf-continuous-scroll-mode.el")
  :hook
  (pdf-view-mode-hook . pdf-continuous-scroll-mode))

;; (use-package nov
;;   :mode ("\\.epub\\'" . nov-mode))


;;;; Log Files

;; Dependency for `logview'.
(use-package datetime
  :custom
  (datetime-timezone 'US/Pacific))

;; TODO Why is this loaded at startup?
(use-package logview
  :mode ("\\.log.*" . logview-mode)
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

(use-package journalctl-mode
  :commands
  journalctl
  journalctl-unit)


;;;; Docker

(use-package dockerfile-mode
  :mode "\\`Dockerfile")

(use-package docker
  :bind
  ("C-c M-d" . docker))

(use-package docker-tramp
  :defer 14
  :config
  (docker-tramp-add-method))

;; dw (https://gitlab.com/mnewt/dw)
(add-to-list 'auto-mode-alist '("DWfile\\'" . sh-mode))
(add-to-list 'interpreter-mode-alist '("dw" . sh-mode))


;;;; Web

;; use imagemagick, if available
;; (when (fboundp 'imagemagick-register-types)
;;   (imagemagick-register-types))

(use-package shr-tag-pre-highlight
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight)))

(use-package eww
  :config
  (defun eww-other-window (url)
    "Fetch URL and render the page.

Open the `eww' buffer in another window."
    (interactive
     (let* ((uris (eww-suggested-uris))
            (prompt (concat "Enter URL or keywords"
                            (if uris (format " (default %s)" (car uris)) "")
                            ": ")))
       (list (read-string prompt nil 'eww-prompt-history uris))))
    (switch-to-buffer-other-window (current-buffer))
    (eww url t))

  :commands
  eww-other-window

  :bind
  ("M-m e" . eww)
  ("M-m E" . eww-other-window))

(use-package w3m
  :custom
  (w3m-search-engine-alist
   '(("google" "https://www.google.com/search?q=%s&ie=utf-8&oe=utf-8&gbv=1" utf-8)
     ("wikipedia" "https://en.wikipedia.org/wiki/Special:Search?search=%s")
     ("duckduckgo" "https://duckduckgo.com/lite&q=%s" utf-8)))
  (w3m-search-default-engine "duckduckgo")
  :commands
  w3m)

(use-package markdown-mode
  :mode "\\.md\\|markdown\\'"
  :custom
  (markdown-list-indent-width tab-width)
  (markdown-command "multimarkdown")
  :bind
  (:map markdown-mode-map
        ("C-c '" . fence-edit-dwim)))

(use-package web-mode
  :mode
  "\\.jsx?\\'"
  "\\.p?html?\\'"
  "\\.tpl\\.php\\'"
  "\\.[agj]sp\\'"
  "\\.as[cp]x\\'"
  "\\.erb\\'"
  "\\.mustache\\'"
  "\\.djhtml\\'"

  :init
  (defun web-mode-setup ()
    "From the `web-mode' FAQ to work with smartparens."
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

  :hook
  (web-mode-hook . lsp-deferred)
  (web-mode-hook . web-mode-setup))

(use-package company-web
  :after company web-mode

  :commands
  company-web-html

  :config
  (defun company-web-setup ()
    "Set up `company-web'."
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-web-html))

  :hook
  (web-mode-hook . company-web-setup))

(use-package css-mode
  :mode "\\.css\\'"
  :custom
  (css-indent-offset tab-width)
  :hook
  (css-mode-hook . lsp-deferred))

;; disable > gem install sass
(use-package sass-mode
  :mode "\\(?:s\\(?:[ac]?ss\\)\\)"
  :hook
  (sass-mode-hook . lsp-deferred))

(use-package emmet-mode
  :custom
  (emmet-expand-jsx-className? t)

  :hook
  ((web-mode-hook sgml-mode-hook css-mode-hook) . emmet-mode))

(use-package restclient
  :mode ("\\.restclient\\'" . restclient-mode)

  :commands
  restclient-outline-mode)

(use-package company-restclient
  :after restclient

  :config
  (defun company-restclient-setup ()
    "Set up `company-restclient'."
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-restclient))

  :hook
  (restclient-mode-hook . company-restclient-setup))

(use-package know-your-http-well
  :defer t
  :after restclient)

;; TODO Make `consult-web'.
;; (use-package counsel-web)


;;;; Javascript

(use-package add-node-modules-path
  :hook
  ((css-mode-hook
    graphql-mode-hook
    js-mode-hook
    markdown-mode-hook
    sass-mode-hook
    web-mode-hook) . add-node-modules-path))

(use-package js
  :mode ("\\.jsx?\\'" . js-mode)
  :custom
  (js-indent-level tab-width)
  :hook
  (js-mode-hook . lsp-deferred))

(use-package json-mode
  :mode ("\\.json\\'" "prettierrc\\'"))

;; > brew install jq
(use-package jq-mode
  :mode "\\.jq$")

(use-package graphql-mode
  :mode "\\.g\\(?:raph\\)?ql\\'")


;;;; Python

(use-package python
  :straight (:type built-in)
  :mode
  ("\\.py\\'" . python-mode)
  ("requirements\\.txt\\'" . conf-mode)
  :interpreter ("python3?" . python-mode)
  :custom
  (gud-pdb-command-name "python -m pdb")
  :bind
  (:map python-mode-map
        ("s-v" . yank)
        ("s-<return>" . python-shell-send-defun)))

;; disable > pip install python-language-server
(use-package lsp-python-ms
  :after python
  :config
  (require 'lsp-python-ms)
  :hook
  (python-mode-hook . lsp-deferred))

(use-package pyvenv
  :after python
  :commands
  pyvenv-activate
  pyvenv-workon)

;; (use-package ein
;;   :commands
;;   ein:run
;;   ein:login)


;;;; Swift

(use-package swift-mode
  :mode "\\.swift"
  :interpreter "swift"
  :hook
  (swift-mode-hook . lsp-deferred))

(use-package lsp-sourcekit
  :config
  (defun lsp-sourcekit-setup ()
    (let ((toolchain (concat "/Applications/Xcode.app/Contents/Developer/Toolchains"
                             "/XcodeDefault.xctoolchain")))
      (setenv "SOURCEKIT_TOOLCHAIN_PATH" toolchain)
      (setq lsp-sourcekit-executable (concat toolchain "/usr/bin/sourcekit-lsp"))))
  :hook
  (swift-mode-hook . lsp-sourcekit-setup))


;;;; Applescript

(use-package apples-mode
  :mode "\\.\\(applescri\\|sc\\)pt\\'"
  :interpreter "osascript"
  :preface
  ;; Allow editing of binary .scpt files (applescript) on mac.
  ;; https://www.emacswiki.org/emacs/AppleScript
  (add-to-list 'jka-compr-compression-info-list
               `["\\.scpt\\'"
                 "converting text applescript to binary applescript "
                 ,(expand-file-name "applescript-helper" "~/.emacs.d/bin/") nil
                 "converting binary applescript to text applescprit "
                 ,(expand-file-name "applescript-helper" "~/.emacs.d/bin/") ("-d")
                 nil t "FasdUAS"])
  ;; It is necessary to perform an update after changing the above variable.
  (jka-compr-update))


;;;; File Modes

;; All modes and mode related stuff which doesn't fit into a larger category.

;; (use-package flyspell
;;   :custom
;;   (ispell-program-name "aspell")
;;   (ispell-extra-args '("--sug-mode=ultra"))
;;   :hook
;;   (text-mode-hook . flyspell-mode)
;;   (prog-mode-hook . flyspell-prog-mode)
;;   :bind
;;   (:map flyspell-mode-map
;;         ("C-," . nil)
;;         ("C-." . nil)
;;         ("C-;" . nil)
;;         ("C-M-i" . nil)))

;; (use-package flyspell-correct-ivy
;;   :after flyspell
;;   :bind
;;   (:map flyspell-mode-map
;;         ([remap flyspell-correct-word-before-point] . flyspell-correct-previous-word-generic)))

(use-package flycheck
  :custom
  (flycheck-idle-change-delay 1)
  (flycheck-mode-line-prefix "")
  (flycheck-emacs-lisp-load-path 'inherit)

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

  :hook
  (prog-mode-hook . flycheck-mode)

  :bind
  ("C-c ! !" . flycheck-mode))

(use-package flycheck-package
  :after flycheck
  :config
  (flycheck-package-setup)
  :bind
  (:map flycheck-mode-map
        ("C-c ! C-l" . package-lint-current-buffer)))

(use-package consult-flycheck
  :bind
  (:map flycheck-command-map
        ("!" . consult-flycheck)))

(use-package lsp-mode
  :custom
  (lsp-enable-snippet t)
  (lsp-auto-guess-root t)
  (lsp-before-save-edits t)
  (lsp-progress-via-spinner nil)

  :config
  ;; Support reading large blobs of data from lsp servers.
  (setq read-process-output-max 1048576) ; 1mb
  (with-eval-after-load 'flycheck
    (add-to-list 'flycheck-checkers #'lsp))

  (defun lsp-reset-mode-line-process ()
    "Reset `mode-line-process' manually after lsp screws it up."
    (interactive)
    (setq mode-line-process nil))

  :hook
  (lsp-after-open-hook . lsp-enable-imenu)
  (lsp-mode-hook . lsp-enable-which-key-integration)

  :bind
  (:map lsp-mode-map
        ("s-l p" . lsp-reset-mode-line-process)
        ("C-h ." . lsp-describe-thing-at-point)))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-position 'top)
  :hook
  (lsp-mode-hook . lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
        ("M-." . lsp-ui-peek-find-definitions)
        ("M-?" . lsp-ui-peek-find-references))
  (:map lsp-ui-peek-mode-map
        ("<return>" . lsp-ui-peek--goto-xref)
        ("M-/" . lsp-ui-peek--goto-xref))
  (:map lsp-command-map
        ("i" . lsp-ui-imenu)))

;; (use-package lsp-ivy
;;   :commands
;;   lsp-ivy-workspace-symbol
;;   lsp-ivy-global-workspace-symbol)

(use-package lsp-treemacs
  :bind
  ("S-l \\'" . lsp-treemacs-symbols))

(use-package consult-lsp
  :bind
  (:map lsp-mode-map
        ([remap xref-find-apropos] . consult-lsp-symbols)))

;; TODO Make this something worth using. See also the competing
;; https://github.com/realgud/realgud, which is an Emacs package to interface
;; with cli debuggers.
;; (use-package dap-mode
;;   :preface
;;   (defun dap-hydra-interactive ()
;;     "Call `dap-hydra' interactively."
;;     (call-interactively #'dap-hydra))

;;   :config
;;   (dap-ui-controls-mode 1)

;;   :hook
;;   (lsp-mode-hook . dap-mode)
;;   (dap-mode-hook . dap-ui-mode)
;;   (dap-ui-mode-hook . dap-ui-controls-mode)
;;   (dap-stopped-hook . dap-hydra-interactive))

(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :defer 11
  :config
  (dolist (formatter '((lua-fmt "luafmt" "--stdin")
                       (rubocop "rubocop" "--format" "emacs" file)
                       (rufo "rufo" "--simple-exit" "--filename" file)
                       (swift-format "xcrun" "swift-format")
                       (shfmt  "shfmt" "-i" "2" "-ci")
                       (xmllint "xmllint" "--format" "-")
                       (zprint "zprint" "{:style :community :map {:comma? false}}")))
    (add-to-list 'apheleia-formatters formatter))

  (dolist (mode '((clojure-mode . zprint)
                  (clojurec-mode . zprint)
                  (clojurescript-mode . zprint)
                  (graphql-mode . prettier)
                  (lua-mode . lua-fmt)
                  (markdown-mode . prettier)
                  (nxml-mode . xmllint)
                  (ruby-mode . rubocop)
                  (sh-mode . shfmt)
                  (swift-mode . swift-format)
                  (web-mode . prettier)))
    (add-to-list 'apheleia-mode-alist mode))

  ;; FIXME This is a naive work in progress. Problems include:
  ;; - Inadvertently trims newlines/whitespace.
  ;; - Doesn't work if the region doesn't parse.
  ;; - Visibly moves point around
  ;; - It's async so if the formatter command takes a long time you could edit
  ;;   it and then lose your edits
  ;; See https://github.com/raxod502/apheleia/issues/11
  (defun apheleia-format-region (start end &optional callback)
    "Format from START to END with `apheleia'."
    (interactive "r")
    (when-let ((command (apheleia--get-formatter-command
                         (if current-prefix-arg
                             'prompt
                           'interactive)))
               (cur-buffer (current-buffer))
               (formatted-buffer (get-buffer-create " *apheleia-formatted*")))
      (with-current-buffer formatted-buffer
        (erase-buffer)
        (insert-buffer-substring-no-properties cur-buffer start end)
        (apheleia-format-buffer
         command
         (lambda ()
           (with-current-buffer cur-buffer
             (delete-region start end)
             (insert-buffer-substring-no-properties formatted-buffer)
             (when callback (funcall callback))))))))

  (defun apheleia-format-defun ()
    "Format the defun with `apheleia'."
    (interactive)
    (let ((pos (point))
          (start (progn (beginning-of-defun) (point)))
          (end (progn (end-of-defun) (point))))
      (apheleia-format-region start end
                              (lambda () (goto-char pos)))))

  (apheleia-global-mode))

(defun indent-buffer ()
  "Indent the buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun format-buffer-or-region (&optional beg end thing)
  "Format the buffer or region, if one is active.

BEG is the beginning of the region.

END is the end of the region.

THING is used to indicate to the user what was just formatted.

Prefix ARG is passed to `fill-paragraph'."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (setq thing (or thing "region"))
      (setq thing (or thing "buffer"))
      (push-mark (or beg (point-min)))
      (push-mark (or end (point-max)) nil t))
    (save-mark-and-excursion
      (when (sp-point-in-string-or-comment) (fill-paragraph current-prefix-arg)))
    (save-mark-and-excursion
      (call-interactively #'crux-cleanup-buffer-or-region))
    (indent-region (region-beginning) (region-end))
    (message "Formatted the %s." thing)))

(defun format-defun-or-region ()
  "Format the current defun or region, if one is active."
  (interactive)
  (save-excursion
    (unless (use-region-p) (mark-defun))
    (format-buffer-or-region (region-beginning) (region-end) 'defun)))

(defun format-line ()
  "Reformat the current line."
  (interactive)
  (format-buffer-or-region (line-beginning-position) (line-end-position) 'line))

(bind-keys
 ("C-M-\\" . format-buffer-or-region)
 ("C-\\" . format-defun-or-region))

(defun maybe-reset-major-mode ()
  "Reset the buffer's `major-mode' if a different mode seems like a better fit.

Mostly useful as a `before-save-hook', to guess mode when saving
a new file for the first time."
  (when (and (eq major-mode 'fundamental-mode)
             (buffer-file-name)
             (not (file-exists-p (buffer-file-name))))
    (normal-mode)))

(use-package sh-script
  :mode ("\\.sh\\'" . sh-mode)
  :interpreter ("\\(?:ba\\|[az]\\)?sh" . sh-mode)
  :custom
  (sh-basic-offset tab-width)
  ;; Tell `executable-set-magic' to insert #!/usr/bin/env interpreter
  (executable-prefix-env t)

  :config
  (defun shell-match-variables-in-quotes (limit)
    "Match variables in double-quotes in `sh-mode' with LIMIT.

https://fuco1.github.io/2017-06-11-Font-locking-with-custom-matchers.html"
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

  (defvar async-shell-command-current-file-history nil)

  (defun async-shell-command-current-file (arg)
    "Run the current file, editing command line if ARG is non-nil.

This command defaults to running the previous command."
    (interactive "P")
    (let ((command (or (car async-shell-command-current-file-history)
                       (concat buffer-file-name " "))))
      (when arg
        (setq command (read-shell-command
                       (if shell-command-prompt-show-cwd
                           (format-message "Async shell command in `%s': "
                                           (abbreviate-file-name default-directory))
                         "Async shell command: ")
                       command
                       'async-shell-command-current-file-history)))
      (async-shell-command command)))

  :hook
  (sh-mode-hook . lsp-deferred)
  (before-save-hook . maybe-reset-major-mode)
  (after-save-hook . executable-make-buffer-file-executable-if-script-p)
  :bind
  (:map sh-mode-map
        ("<return>" . newline-and-indent)
        ("RET" . newline-and-indent)
        ("C-c m" . executable-set-magic)
        ("C-c M-s" . sh-select)
        ("C-c 7" . async-shell-command-current-file)))

(use-package conf-mode
  :mode
  ;; mbsync
  "\\.mbsyncrc"
  ;; pkg-config
  "\\.pc"
  ;; godot
  "\\.\\(tres\\|tscn\\|import\\|godot\\)\\'")

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

(use-package daemons
  :commands
  daemons)

;; DNS
(use-package dns-mode
  :mode "\\.rpz\\'"
  :config
  (defun dns-insert-timestamp ()
    "Insert DNS timestamp."
    (interactive)
    (insert (format-time-string "%Y%m%d%H%M"))))

;; (use-package genrnc
;;   :custom
;;   (genrnc-user-schemas-directory "~/.emacs.d/schema")
;;   :commands
;;   (genrnc-regist-file))

;; (use-package rnc-mode
;;   :mode "\\.rnc\\'")

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package apache-mode
  :mode "/apache2/")

(use-package nginx-mode
  :mode
  "nginx\\.conf\\'"
  "/nginx/.+\\.conf\\'"
  :custom
  (nginx-indent-level tab-width))

(use-package caddyfile-mode
  :mode "Caddyfile.*"
  :config
  (defun caddyfile-setup ()
    "Set up `caddyfile-mode'.

This package sets these explicitly so we have to do the same."
    (setq tab-width 4
          indent-tabs-mode nil))
  :hook
  (caddyfile-mode-hook . caddyfile-setup))

(use-package yaml-mode
  :mode "\\.\\(ya\?ml\\|meta\\|unity\\|prefab\\)\\'"
  :bind
  ;; Don't change ident level when yanking.
  (:map yaml-mode-map
        ("s-v" . clipboard-yank)
        ("RET" . newline-and-indent)))

(use-package toml-mode
  :mode
  "\\.toml\\'"
  "Pipfile\\'")

;; > gem install rufo
(use-package ruby-mode
  :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
  :hook
  (ruby-mode-hook . lsp-deferred))

(use-package inf-ruby
  :after ruby-mode
  :hook
  (ruby-mode-hook . inf-ruby-minor-mode)
  (compilation-filter . inf-ruby-auto-enter)
  :bind
  (:map inf-ruby-minor-mode-map
        ("s-<return>". ruby-send-last-sexp)
        ("C-M-x" . ruby-send-block)))

;; disable > brew install lua luarocks
;; luarocks install --server=https://luarocks.org/dev lua-lsp --local
(use-package lua-mode
  :mode "\\.lua\\'"
  :custom
  (lua-indent-level tab-width)
  :hook
  (lua-mode-hook . lsp-deferred))

(use-package fennel-mode
  :mode "\\.fnl\\'")

(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-indent-offset tab-width)
  :hook
  (rust-mode-hook . lsp-deferred))

(use-package go-mode
  :mode "\\.go\\'")

(use-package company-go
  :after go-mode
  :config
  (defun company-go-setup ()
    "Set up `company-go'."
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-go))
  :hook
  (go-mode-hook . company-go-setup)
  (go-mode-hook . lsp-deferred))

;; Install on macOS:
;; > brew install powershell
(use-package powershell
  :mode "\\.ps1\\'"
  :custom
  (powershell-indent tab-width)
  (powershell-continuation-indent tab-width)
  :hook
  (powershell-mode-hook . lsp-deferred))

(use-package php-mode
  :mode "\\.php\\'"
  :hook
  (php-mode-hook . lsp-deferred))

(use-package ios-config-mode
  :straight (ios-config-mode :host github :repo "mnewt/IOS-config-mode")
  :mode "\\.cfg\\'")

(use-package cc-mode
  :custom
  (c-basic-offset 4)
  :hook
  ((c-mode-hook c++-mode-hook java-mode-hook) . lsp-deferred)
  :bind
  (:map c-mode-map
        ("<" . c-electric-lt-gt)
        (">" . c-electric-lt-gt)))

(use-package nxml-mode
  :straight (:type built-in)
  :hook
  (nxml-mode-hook . lsp-deferred))

(use-package csharp-mode
  :mode "\\.cs\\'"

  :config
  (add-to-list 'c-default-style '(csharp-mode . "c#"))

  (defun csharp-mode-setup ()
    "Set up C# mode."
    (exec-path-from-shell-setenv
     "PATH"
     (concat "/Library/Frameworks/Mono.framework/Versions/Current/Commands" ":"
             (exec-path-from-shell-getenv "PATH")))
    (setq c-syntactic-indentation t)
    (setq truncate-lines t))

  (defvar csharp-mode-map)

  :hook
  (csharp-mode-hook . csharp-mode-setup)

  :bind
  (:map csharp-mode-map
        ("<" . c-electric-lt-gt)
        (">" . c-electric-lt-gt)))

(use-package omnisharp
  :custom
  (omnisharp-imenu-support t)
  ;; Use `omnisharp-install-server' to set things up after installing the
  ;; package.
  :config
  (defvar omnisharp-mode-map)

  (defun omnisharp-mode-setup ()
    "Set up C# mode."
    (omnisharp-install-server nil)
    (omnisharp-mode)
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends #'company-omnisharp)
    (add-hook 'before-save-hook #'omnisharp-code-format-entire-file))

  :hook
  (csharp-mode-hook . omnisharp-mode-setup)

  :bind
  (:map omnisharp-mode-map
        ("C-c r" . omnisharp-run-code-action-refactoring)
        ("C-c C-c" . recompile)))

(use-package ahk-mode
  :mode "\\.ahk\\'")

;; > brew install plantuml
(use-package plantuml-mode
  :mode "\\.puml\\'"
  :custom
  ;; The server doesn't work right because of encoding problems.
  ;; TODO: File a bug report
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path
   (car (file-expand-wildcards "/usr/local/Cellar/plantuml/*/libexec/plantuml.jar")))
  (plantuml-indent-level tab-width)
  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

  (defvar org-plantuml-jar-path)
  (setq org-plantuml-jar-path plantuml-jar-path)

  (defun plantuml-completion-at-point ()
    "Function used for `completion-at-point-functions' in `plantuml-mode'."
    (let ((completion-ignore-case t)    ; Not working for company-capf.
          (bounds (bounds-of-thing-at-point 'symbol))
          (keywords plantuml-kwdList))
      (when (and bounds keywords)
        (list (car bounds)
              (cdr bounds)
              keywords
              :exclusve 'no
              :company-docsig #'identity))))

  (defun plantuml-completion-at-point-setup ()
    "Set up `completion-at-point' for plantuml-mode."
    (setq plantuml-output-type "png")
    (add-hook 'completion-at-point-functions
              #'plantuml-completion-at-point nil 'local))

  :hook
  (plantuml-mode-hook . plantuml-completion-at-point-setup))

(use-package flycheck-plantuml
  :hook
  (plantuml-mode-hook . flycheck-plantuml-setup))

(use-package mermaid-mode
  :mode "\\.mmd\\'")

(use-package ob-mermaid
  :functions
  org-babel-execute:mermaid)

(use-package nftables-mode
  :straight (:host github :repo "mnewt/nftables-mode")
  :mode ("\\.nft\\(?:ables\\)?\\'" "/etc/nftables.conf")
  :interpreter "nft\\(?:ables\\)?")

(use-package gdscript-mode
  :straight (:type git :host github :repo "godotengine/emacs-gdscript-mode")
  :mode "\\.gd\\'"

  :config
  ;; FIXME
  ;; https://github.com/emacs-lsp/lsp-mode/issues/2127
  ;; (require 'lsp-gdscript)
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-gdscript-tcp-connect-to-port)
  ;;                   :major-modes '(gdscript-mode)
  ;;                   :server-id 'gdscript
  ;;                   ;; Ignore unsupported messages.
  ;;                   :notification-handlers (lsp-ht
  ;;                                           ("gdscript/capabilities" 'ignore)
  ;;                                           ("textDocument/publishDiagnostics" 'ignore)
  ;;                                           ("executeCommand" 'ignore))))

  :hook
  (gdscript-mode-hook . lsp-deferred))

(use-package yasnippet-godot-gdscript
  :straight (:type git :host github :repo "francogarcia/yasnippet-godot-gdscript")
  :after gdscript-mode
  :init
  ;; FIXME the directory name doesn't match the major mode.
  (with-eval-after-load 'yasnippet
    (add-to-list 'yas-snippet-dirs
                 (straight--repos-dir "yasnippet-godot-gdscript" "snippets"))))

(use-package vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")


;;;; Multiple Major Modes

;; TODO: `polymode' with this config causes rjsx files to explode, it's
;; really bad.
;;
;; (run-with-timer 8 nil (lambda () (require 'polymode-setup)))

(use-package fence-edit
  :straight (fence-edit :host github :repo "aaronbieber/fence-edit.el")
  :config
  (setq fence-edit-blocks
        (append '(("---" "---" yaml)
                  ("+++" "+++" toml)
                  ("graphql[ \t\n]*(?`" "`" graphql)
                  ("<svg" "</svg>" nxml t)
                  ("<html" "</html>" web t)
                  ("<style type=\"text/css\">" "</style>" css)
                  ("<div" "</div>" web t)
                  ;; TODO: How to ignore escaped double quotes? (`\"') polymode
                  ;; uses functions to determine start and end. That's probably
                  ;; necessary since regexp are no good handling symantic
                  ;; elements like quotes. But maybe there is a regexp that is a
                  ;; good enough hack?
                  ("do-applescript\s-*.*\"" "\"" apples))
                fence-edit-blocks))

  (with-eval-after-load 'markdown-mode
    (bind-keys :map markdown-mode-map
               ("C-c '" . nil)))
  :bind
  ("C-c '" . fence-edit-dwim))


;;;; Org

(use-package org
  :defer 17
  :mode ("\\.org\\'" . org-mode)
  :custom
  ;; Indent text according to the outline structure (`org-indent-mode')
  ;; (org-startup-indented t)
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
  (org-export-with-section-numbers nil)
  ;; (org-ellipsis "...")
  ;; Customize todo keywords
  (org-todo-keywords '((sequence "TODO(t)" "WORK(w)" "WAIT(a)" "|" "DONE(d!)")
                       (sequence "|" "CANCELED(c)")))
  (org-todo-keyword-faces
   '(("TODO" . (:background "magenta" :foreground "white" :weight bold))
     ("WORK" . (:background "green" :foreground "black" :weight bold))
     ("WAIT" . (:background "orange" :foreground "black" :weight bold))
     ("DONE" . (:background "gray" :foreground "black" :weight bold))
     ("CANCELED" . (:background "gray" :foreground "black" :weight bold :strike-through t))))
  (org-catch-invisible-edits 'show-and-error)
  (org-capture-templates
   `(("t"
      "TODO"
      entry
      (file ,(expand-file-name "TODO.org" org-directory))
      "* TODO %?\n  %i\n  %a")
     ("n"
      "Note"
      entry
      (function org-find-file-for-capture)
      "* %?\n  %i\n  %a")
     ("m"
      "Meeting Notes"
      entry
      (function org-find-file-for-meeting-notes)
      "%(format \"#+TITLE: %s\n\n* Attendees\n\n*Agenda\n\n* Notes\n\" org-capture-meeting-subject)")
     ("e"
      "TODO respond to email"
      entry
      (file ,(expand-file-name "TODO.org" org-directory))
      "* TODO %^{Description}\n%A\n%?\n")
     ("j"
      "Journal Entry"
      entry
      (file ,(expand-file-name (format "journal/%s.org" (format-time-string "%F"))
                               org-directory))
      "* %<%I:%M %p>\n   %?")))
  ;; Don't prompt to confirm if I want to evaluate a source block
  (org-confirm-babel-evaluate nil)
  (org-startup-with-inline-images "inlineimages")
  (org-image-actual-width 500)
  ;; When exporting to odt, actually create a docx.
  (org-odt-preferred-output-format "docx")
  (org-return-follows-link t)

  :commands
  org-todo
  org-entry-get
  org-sort-entries
  org-map-entries
  org-capture
  org-capture-refile

  :config

  ;; KLUDGE Redefining these internal functions is a workaround for `straight'
  ;; not setting running make to set the proper variables during the build
  ;; process.
  (defun m-org-release ()
    "9.5-dev")

  (advice-add #'org-release :override #'m-org-release)

  (defun m-org-git-version ()
    "Via straight.el")

  (advice-add #'org-git-version :override #'m-org-git-version)

  (defun org-find-file-for-capture (&optional file)
    "Open a file and ready it for capture."
    (let ((default-directory org-directory))
      (if file
          (find-file file)
        (call-interactively #'find-file)))
    (org-with-wide-buffer
     (goto-char (point-max))
     (while (not (looking-back "\n\n" nil))
       (insert "\n"))))

  (defun org-find-file-for-meeting-notes ()
    "Open a file and ready it for meeting notes."
    (org-find-file-for-capture
     (setq org-capture-meeting-subject
           (format "justis/%s-meeting-%s"
                   (format-time-string "%F")
                   (read-string "Meeting Subject: ")))))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((awk . t)
                                 (calc . t)
                                 (clojure . t)
                                 (emacs-lisp . t)
                                 (js . t)
                                 (lisp . t)
                                 (mermaid . t)
                                 (perl . t)
                                 (plantuml . t)
                                 (python . t)
                                 (ruby . t)
                                 (scheme . t)
                                 (sed . t)
                                 (shell . t)
                                 (sql . t)
                                 (sqlite . t)))

  (defvar org-babel-clojure-backend)
  (setq org-babel-clojure-backend 'cider)

  (defun window-config-org ()
    "Set up Org window config."
    (interactive)
    (delete-other-windows)
    (dired org-directory)
    (switch-to-buffer-other-window (current-buffer))
    (find-file (expand-file-name "TODO.org" org-directory))
    (other-window 1))

  (defun org-todo-file (arg)
    "Open the appropriate Org TODO.org file, informed by ARG.

When in a project directory, open it in the project root.

When not in a project directory or a prefix ARG is specified,
open it in `org-directory'."
    (interactive "P")
    (find-file (expand-file-name "TODO.org"
                                 (if (and (not arg) (projectile-project-p))
                                     (projectile-project-root)
                                   org-directory))))

  (defun org-search-org-directory ()
    "Search ~/org using `ripgrep'."
    (interactive)
    (let ((default-directory org-directory))
      (consult-ripgrep)))

  (defun org-new-note (arg)
    "Create a new Org buffer with ARG controlling the location.

Without prefix ARG, create it in the project root directory (if
we are in a project) or the current directory.

With a prefix ARG, create it in `org-directory'."
    (interactive "P")
    (find-file (expand-file-name "new-note.org"
                                 (cond
                                  (arg org-directory)
                                  ((projectile-project-p)
                                   (projectile-project-root))))))

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

  (defvar org-default-priority)

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
    (require 'org-archive)
    (require 'outline)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/DONE" 'file))

  (defface org-emphasis-marker '((t (:inherit shadow)))
    "Face for Org emphasis markers"
    :group 'org-faces)

  (defvar org-element-paragraph-separate)
  ;; This is a re-definition of a built in function.
  ;; TODO Follow up with Org mailing list on this approach.
  (defun org-do-emphasis-faces-improved (limit)
    "Run through the buffer and emphasize strings."
    (require 'org-macs)
    (require 'org-compat)
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

              ;; Begin new code
              (font-lock-prepend-text-property
               (match-beginning 3) (match-end 3) 'face 'org-emphasis-marker)
              (font-lock-prepend-text-property
               (match-end 4) (match-beginning 5) 'face 'org-emphasis-marker)
              ;; End new code

              (when org-hide-emphasis-markers
                (add-text-properties (match-end 4) (match-beginning 5)
                                     '(invisible org-link))
                (add-text-properties (match-beginning 3) (match-end 3)
                                     '(invisible org-link))))
            (throw :exit t))))))

  (advice-add #'org-do-emphasis-faces :override #'org-do-emphasis-faces-improved)

  (defun org-show-only-current-subtree (&rest _)
    "Fold all other trees, then show entire current subtree."
    (interactive)
    (org-overview)
    (org-reveal)
    (org-show-subtree))

  (defun org-link-message ()
    "When in `org-mode', display link destinations in the minibuffer."
    (when (derived-mode-p 'org-mode)
      (let ((object (org-element-context)))
        (when (eq (car object) 'link)
          (message "%s" (org-element-property :raw-link object))))))

  ;; (use-package org-spacer
  ;;   :straight (:host github :repo "dustinlacewell/org-spacer.el")
  ;;   :config
  ;;   (defun org-spacer-setup ()
  ;;     "Set up `org-spacer'."
  ;;     (add-hook 'before-save-hook 'org-spacer-enforce nil 'local))
  ;;   :hook
  ;;   (org-mode-hook . org-spacer-setup))

  (defun org-fix-blank-lines ()
    "Ensure blank lines exist between top level headings.

Adapted from
https://github.com/alphapapa/unpackaged.el/blob/master/unpackaged.el."
    (interactive)
    (org-map-entries (lambda ()
                       (when (>= 3 (org-current-level))
                         (org-with-wide-buffer
                          ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                          ;; newlines before the current heading, so we do this part widened.
                          (while (not (looking-back "\n\\(?:[[:space:]]+\\)?\n" nil))
                            ;; Insert blank lines before heading.
                            (insert "\n")))))
                     t nil))

  (defvar org-odt-convert-processes)

  (defun org-mode-setup ()
    "Set up `org-mode'."
    ;; Only in `org-mode', disable some smartparens bindings by making an
    ;; overriding keymap.
    (when (and (derived-mode-p 'org-mode) smartparens-mode)
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map smartparens-mode-map)
        (define-key map (kbd "C-M-u") nil)
        (define-key map (kbd "C-M-d") nil)
        (push `(smartparens-mode . ,map) minor-mode-overriding-map-alist)))

    ;; Configure LibreOffice ODT export to actually export to MS Office format.
    (let ((cmd "/Applications/LibreOffice.app/Contents/MacOS/soffice"))
      (when (and (eq system-type 'darwin) (file-exists-p cmd))
        (setq org-odt-convert-processes
              '(("LibreOffice"
                 "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")))))

    (add-hook 'before-save-hook #'org-fix-blank-lines 90 'local)

    (add-hook 'post-command-hook #'org-link-message 90 'local))

  :hook
  (org-mode-hook . org-mode-setup)
  (org-babel-after-execute-hook . org-redisplay-inline-images)

  :bind
  ("C-c C-o" . org-open-at-point)
  ("C-c l" . org-store-link)
  ("C-c C-l" . org-insert-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switchb)
  (:map org-mode-map
        ("C-e" . org-end-of-line)
        ("C-M-}" . org-forward-sentence)
        ("C-M-{" . org-backward-sentence)
        ("M-S-<up>" . org-move-subtree-up)
        ("M-S-<down>" . org-move-subtree-down)
        ("s->" . org-shiftright)
        ("s-<" . org-shiftleft)
        ("M-p" . org-backward-heading-same-level)
        ("M-n" . org-forward-heading-same-level)
        ("C-M-u" . org-up-element)
        ("C-M-d" . org-down-element)
        ("C-s-t" . org-show-only-current-subtree)
        ("C-M-u" . org-up-element)
        ("C-M-d" . org-down-element))
  (:map m-org-map
        ("a" . org-agenda)
        ("b" . org-switchb)
        ("c" . org-capture)
        ("d" . persp-switch-to-org)
        ("i" . org-insert-link)
        ("l" . org-store-link)
        ("n" . org-new-note)
        ("o" . org-overview)
        ("s" . org-search-org-directory)
        ("t" . org-todo-file))
  (:map m-search-map
        ("o" . org-search-org-directory))
  (:map m-file-map
        ("o" . persp-switch-to-org))
  (:map visual-line-mode-map
        ;; Don't shadow mwim and org-mode bindings
        ([remap move-beginning-of-line] . nil)))

(use-package ob-async
  :after org)

(use-package ob-session-async
  :after org
  :straight (:host github :repo "jackkamm/ob-session-async"))

(use-package org-download
  :after org
  :hook
  (dired-mode-hook . org-download-enable))

(use-package htmlize
  :after org
  :commands
  htmlize-buffer
  htmlize-file)

(use-package org-preview-html
  :after org
  :commands
  org-preview-html-mode)

;; TODO Get this to work.
;; (use-package inherit-org
;;   :straight (:host github :repo "chenyanming/inherit-org")
;;   :after org
;;   :config
;;   (with-eval-after-load 'info
;;     (add-hook 'Info-mode-hook 'inherit-org-mode))

;;   (with-eval-after-load 'helpful
;;     (add-hook 'helpful-mode-hook 'inherit-org-mode))

;;   (with-eval-after-load 'w3m
;;     (add-hook 'w3m-fontify-before-hook 'inherit-org-w3m-headline-fontify)
;;     (add-hook 'w3m-fontify-after-hook 'inherit-org-mode)))

;; Install:
;; disable > brew install tclap
(use-package notdeft
  :straight (:host github :repo "hasu/notdeft")
  :custom
  (notdeft-directory org-directory)
  (notdeft-directories `(,org-directory))
  (notdeft-secondary-extensions nil)
  :bind
  ("C-c n s" . notdeft))

(use-package poporg
  :bind
  ("C-c C-'" . poporg-dwim))

(use-package orglink
  :defer 15
  :hook
  (prog-mode-hook . orglink-mode))

;; `ov' is an implicit dependency of `ox-clip'.
(use-package ov)

(use-package ox-clip
  :after ov
  :config
  :bind
  ("M-m o w" . ox-clip-formatted-copy)
  ("M-m o W" . ox-clip-image-to-clipboard))

(use-package math-at-point
  :straight (:host github :repo "shankar2k/math-at-point")
  :bind
  ("C-c C-=" . math-at-point))


;;;; Hydra

;; (use-package hydra
;;   :defer 10
;;   :functions
;;   hydra-default-pre
;;   hydra-keyboard-quit
;;   hydra--call-interactively-remap-maybe
;;   hydra-show-hint
;;   hydra-set-transient-map

;;   :config
;;   (radian-protect-macros

;;     (with-eval-after-load 'hs
;;       (defhydra hydra-hs (:color pink :hint nil)
;;         "
;; Hideshow

;; Hide^^            ^Show^            ^Toggle^    ^Navigation^
;; ----------------------------------------------------------------
;; _h_ hide all      _s_ show all      _t_ toggle    _n_ next line
;; _d_ hide block    _a_ show block                _p_ previous line
;; _l_ hide level

;; _q_ quit
;; "
;;         ("s" hs-show-all)
;;         ("h" hs-hide-all)
;;         ("a" hs-show-block)
;;         ("d" hs-hide-block)
;;         ("t" hs-toggle-hiding)
;;         ("l" hs-hide-level)
;;         ("n" forward-line)
;;         ("p" (forward-line -1))
;;         ("q" nil))

;;       (bind-key (kbd "C-c h") #'hydra-hs/body hs-minor-mode-map))

;;     (with-eval-after-load 'outline
;;       (defhydra hydra-outline (:color pink :hint nil)
;;         "
;; Outline

;; ^Hide^             ^Show^           ^Move
;; ^^^^^^------------------------------------------------------
;; _s_ sublevels     _a_ all         _u_ up
;; _t_ body          _e_ entry       _n_ next visible
;; _o_ other         _i_ children    _p_ previous visible
;; _c_ entry         _k_ branches    _f_ forward same level
;; _l_ leaves        _s_ subtree     _b_ backward same level
;; _d_ subtree

;; "
;;         ;; Hide
;;         ("q" outline-hide-sublevels)
;;         ("t" outline-hide-body)
;;         ("o" outline-hide-other)
;;         ("c" outline-hide-entry)
;;         ("l" outline-hide-leaves)
;;         ("d" outline-hide-subtree)
;;         ;; Show
;;         ("a" outline-show-all)
;;         ("e" outline-show-entry)
;;         ("i" outline-show-children)
;;         ("k" outline-show-branches)
;;         ("s" outline-show-subtree)
;;         ;; Move
;;         ("u" outline-up-heading)
;;         ("n" outline-next-visible-heading)
;;         ("p" outline-previous-visible-heading)
;;         ("f" outline-forward-same-level)
;;         ("b" outline-backward-same-level)
;;         ("q" nil "leave"))

;;       (bind-key (kbd "C-c #") #'hydra-outline/body outline-minor-mode-map))

;;     (with-eval-after-load 'ibuffer
;;       (defhydra hydra-ibuffer-main (:color pink :hint nil)
;;         "
;;     ^Mark^         ^Actions^         ^View^          ^Select^              ^Navigation^
;;     _m_ mark      _D_ delete       _g_ refresh    _q_ quit             _k_   ↑    _h_
;;     _u_ unmark    _s_ save marked  _S_ sort       _TAB_ toggle         _RET_ visit
;;     _*_ specific  _a_ all actions  _/_ filter     _o_ other window     _j_   ↓    _l_
;;     _t_ toggle    _._ toggle hydra _H_ help       C-o other win no-select
;;     "
;;         ("m" ibuffer-mark-forward)
;;         ("u" ibuffer-unmark-forward)
;;         ("*" hydra-ibuffer-mark/body :color blue)
;;         ("t" ibuffer-toggle-marks)

;;         ("D" ibuffer-do-delete)
;;         ("s" ibuffer-do-save)
;;         ("a" hydra-ibuffer-action/body :color blue)

;;         ("g" ibuffer-update)
;;         ("S" hydra-ibuffer-sort/body :color blue)
;;         ("/" hydra-ibuffer-filter/body :color blue)
;;         ("H" describe-mode :color blue)

;;         ("h" ibuffer-backward-filter-group)
;;         ("k" ibuffer-backward-line)
;;         ("l" ibuffer-forward-filter-group)
;;         ("j" ibuffer-forward-line)
;;         ("RET" ibuffer-visit-buffer :color blue)

;;         ("TAB" ibuffer-toggle-filter-group)

;;         ("o" ibuffer-visit-buffer-other-window :color blue)
;;         ("q" quit-window :color blue)
;;         ("." nil :color blue))

;;       (defhydra hydra-ibuffer-mark (:color teal :columns 5
;;                                            :after-exit (hydra-ibuffer-main/body))
;;         "Mark"
;;         ("*" ibuffer-unmark-all "unmark all")
;;         ("M" ibuffer-mark-by-mode "mode")
;;         ("m" ibuffer-mark-modified-buffers "modified")
;;         ("u" ibuffer-mark-unsaved-buffers "unsaved")
;;         ("s" ibuffer-mark-special-buffers "special")
;;         ("r" ibuffer-mark-read-only-buffers "read-only")
;;         ("/" ibuffer-mark-dired-buffers "dired")
;;         ("e" ibuffer-mark-dissociated-buffers "dissociated")
;;         ("h" ibuffer-mark-help-buffers "help")
;;         ("z" ibuffer-mark-compressed-file-buffers "compressed")
;;         ("b" hydra-ibuffer-main/body "back" :color blue))

;;       (defhydra hydra-ibuffer-action (:color teal :columns 4
;;                                              :after-exit
;;                                              (if (eq major-mode 'ibuffer-mode)
;;                                                  (hydra-ibuffer-main/body)))
;;         "Action"
;;         ("A" ibuffer-do-view "view")
;;         ("E" ibuffer-do-eval "eval")
;;         ("F" ibuffer-do-shell-command-file "shell-command-file")
;;         ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
;;         ("H" ibuffer-do-view-other-frame "view-other-frame")
;;         ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
;;         ("M" ibuffer-do-toggle-modified "toggle-modified")
;;         ("O" ibuffer-do-occur "occur")
;;         ("P" ibuffer-do-print "print")
;;         ("Q" ibuffer-do-query-replace "query-replace")
;;         ("R" ibuffer-do-rename-uniquely "rename-uniquely")
;;         ("T" ibuffer-do-toggle-read-only "toggle-read-only")
;;         ("U" ibuffer-do-replace-regexp "replace-regexp")
;;         ("V" ibuffer-do-revert "revert")
;;         ("W" ibuffer-do-view-and-eval "view-and-eval")
;;         ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
;;         ("b" nil "back"))

;;       (defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
;;         "Sort"
;;         ("i" ibuffer-invert-sorting "invert")
;;         ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
;;         ("v" ibuffer-do-sort-by-recency "recently used")
;;         ("s" ibuffer-do-sort-by-size "size")
;;         ("f" ibuffer-do-sort-by-filename/process "filename")
;;         ("m" ibuffer-do-sort-by-major-mode "mode")
;;         ("b" hydra-ibuffer-main/body "back" :color blue))

;;       (defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
;;         "Filter"
;;         ("m" ibuffer-filter-by-used-mode "mode")
;;         ("M" ibuffer-filter-by-derived-mode "derived mode")
;;         ("n" ibuffer-filter-by-name "name")
;;         ("c" ibuffer-filter-by-content "content")
;;         ("e" ibuffer-filter-by-predicate "predicate")
;;         ("f" ibuffer-filter-by-filename "filename")
;;         (">" ibuffer-filter-by-size-gt "size")
;;         ("<" ibuffer-filter-by-size-lt "size")
;;         ("/" ibuffer-filter-disable "disable")
;;         ("b" hydra-ibuffer-main/body "back" :color blue))

;;       (bind-key (kbd ".") #'hydra-ibuffer-main/body ibuffer-mode-map))

;;     (with-eval-after-load 'dired
;;       (defhydra hydra-dired (:hint nil :color pink)
;;         "
;;   _+_ mkdir          _v_ view         _m_ mark             _(_ details        _i_ insert-subdir
;;   _C_ copy           _O_ view other   _U_ unmark all       _)_ omit-mode      _W_  wdired
;;   _D_ delete         _o_ open other   _u_ unmark           _l_ redisplay      _w_ kill-subdir
;;   _R_ rename         _M_ chmod        _t_ toggle           _g_ revert buf     _e_ ediff
;;   _Y_ rel symlink    _G_ chgrp        _E_ extension mark   _s_ sort           _r_ rsync
;;   _S_ symlink        _z_ compress     _F_ find marked                       _?_ summary
;;   _A_ find regexp    _Q_ repl regexp                                      _q_ quit

;;   C-x C-q : edit     C-c C-c : commit C-c ESC : abort                 _._ toggle hydra
;;   "
;;         ("(" dired-hide-details-mode)
;;         (")" dired-omit-mode)
;;         ("+" dired-create-directory)
;;         ("?" dired-summary)
;;         ("A" dired-do-find-regexp)
;;         ("C" dired-do-copy)
;;         ("D" dired-do-delete)
;;         ("E" dired-mark-extension)
;;         ("e" dired-ediff-files)
;;         ("F" dired-do-find-marked-files)
;;         ("G" dired-do-chgrp)
;;         ("g" revert-buffer)
;;         ("i" dired-maybe-insert-subdir)
;;         ("l" dired-do-redisplay) ;; relist the marked or single directory
;;         ("M" dired-do-chmod)
;;         ("m" dired-mark)
;;         ("O" dired-display-file)
;;         ("o" dired-find-file-other-window)
;;         ("Q" dired-do-find-regexp-and-replace)
;;         ("R" dired-do-rename)
;;         ("r" dired-rsync)
;;         ("S" dired-do-symlink)
;;         ("s" dired-sort-toggle-or-edit)
;;         ("t" dired-toggle-marks)
;;         ("U" dired-unmark-all-marks)
;;         ("u" dired-unmark)
;;         ("v" dired-view-file) ;; q to exit, s to search, = gets line #
;;         ("w" dired-kill-subdir)
;;         ("W" wdired-change-to-wdired-mode)
;;         ("Y" dired-do-relsymlink)
;;         ("z" dired-do-compress)
;;         ("q" nil)
;;         ("." nil :color blue))

;;       (bind-keys :map dired-mode-map
;;                  ("." . hydra-dired/body)
;;                  ("C-c C-s" . hydra-dired-quick-sort/body)))

;;     (autoload #'windmove-find-other-window "windmove")
;;     (with-eval-after-load 'windmove
;;       (defun hydra-move-splitter-left (arg)
;;         "Move window splitter left by ARG characters."
;;         (interactive "p")
;;         (if (windmove-find-other-window 'right)
;;             (shrink-window-horizontally arg)
;;           (enlarge-window-horizontally arg)))

;;       (defun hydra-move-splitter-right (arg)
;;         "Move window splitter right by ARG characters."
;;         (interactive "p")
;;         (if (windmove-find-other-window 'right)
;;             (enlarge-window-horizontally arg)
;;           (shrink-window-horizontally arg)))

;;       (defun hydra-move-splitter-up (arg)
;;         "Move window splitter up by ARG characters."
;;         (interactive "p")
;;         (if (windmove-find-other-window 'up)
;;             (enlarge-window arg)
;;           (shrink-window arg)))

;;       (defun hydra-move-splitter-down (arg)
;;         "Move window splitter down by ARG characters."
;;         (interactive "p")
;;         (if (windmove-find-other-window 'up)
;;             (shrink-window arg)
;;           (enlarge-window arg)))

;;       (defhydra hydra-window (:hint nil)
;;         "
;;   MOVE WINDOW^   _h_ left          _j_ down                    _k_ up             _l_ right
;;   MOVE BUFFER^   _←_ left          _↓_ down                    _↑_ up             _→_ right
;;   SPLIT^         _V_ vertical      _H_ horizontal              _u_ undo           _r_ redo
;;   SIZE^          _b_ thinner       _n_ taller                  _p_ shorter        _f_ wider                 _B_ balance
;;   DELETE^        _d_ kill buffer   _D_ kill buffer and window  _w_ delete window  _W_ delete other windows
;;                 _q_ quit
;;   "
;;         ("h" windmove-left)
;;         ("j" windmove-down)
;;         ("k" windmove-up)
;;         ("l" windmove-right)
;;         ("<left>" buf-move-left)
;;         ("<down>" buf-move-down)
;;         ("<up>" buf-move-up)
;;         ("<right>" buf-move-right)
;;         ("V" (lambda () (interactive) (split-window-right) (windmove-right)))
;;         ("H" (lambda () (interactive) (split-window-below) (windmove-down)))
;;         ("u" (progn (winner-undo) (setq this-command 'winner-undo)))
;;         ("r" winner-redo)
;;         ("b" hydra-move-splitter-left)
;;         ("n" hydra-move-splitter-down)
;;         ("p" hydra-move-splitter-up)
;;         ("f" hydra-move-splitter-right)
;;         ("B" balance-windows)
;;         ("d" kill-current-buffer)
;;         ("D" kill-buffer-and-window)
;;         ("w" delete-window)
;;         ("W" delete-other-windows)
;;         ("q" nil))

;;       (defhydra hydra-move (:hint nil)
;;         "
;;   hydra-move: [_n_ _N_ _p_ _P_ _v_ _V_ _u_ _d_] [_f_ _F_ _b_ _B_ _a_ _A_ _e_ _E_] [_,_ _._ _l_ _c_] _q_"
;;         ("n" next-line)
;;         ("N" scroll-down-margin)
;;         ("p" previous-line)
;;         ("P" scroll-up-margin)
;;         ("v" scroll-up-command)
;;         ("V" scroll-down-command)
;;         ("u" scroll-window-up)
;;         ("d" scroll-window-down)
;;         ("f" forward-char)
;;         ("F" forward-word)
;;         ("b" backward-char)
;;         ("B" backward-word)
;;         ("a" mwim-beginning-of-code-or-line)
;;         ("A" beginning-of-defun)
;;         ("e" mwim-end-of-code-or-line)
;;         ("E" end-of-defun)
;;         ("," beginning-of-buffer)
;;         ("." end-of-buffer)
;;         ("l" recenter-top-bottom)
;;         ("c" goto-last-change)
;;         ("q" nil)))

;;     (with-eval-after-load 'flycheck
;;       (defhydra hydra-flycheck
;;         (:pre (progn (setq hydra-hint-display-type t) (flycheck-list-errors))
;;               :post (progn (setq hydra-hint-display-type nil)
;;                            (quit-windows-on "*Flycheck errors*"))
;;               :hint nil)
;;         "Errors"
;;         ("f" flycheck-error-list-set-filter "Filter")
;;         ("n" flycheck-next-error "Next")
;;         ("p" flycheck-previous-error "Previous")
;;         ("<" flycheck-first-error "First")
;;         (">" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
;;         ("q" nil))

;;       (bind-key (kbd "C-c ! .") #'hydra-flycheck/body flycheck-mode-map))

;;     (with-eval-after-load 'smerge
;;       (defhydra hydra-smerge (:color pink :hint nil :post (smerge-auto-leave))
;;         "
;; ^Move^       ^Keep^               ^Diff^                 ^Other^
;; ^^-----------^^-------------------^^---------------------^^-------
;; _n_ext       _b_ase               _<_: upper/base        _C_ombine
;; _p_rev       _u_pper              _=_: upper/lower       _r_esolve
;; ^^           _l_ower              _>_: base/lower        _k_ill current
;; ^^           _a_ll                _R_efine
;; ^^           _RET_: current       _E_diff
;; "
;;         ("n" smerge-next)
;;         ("p" smerge-prev)
;;         ("b" smerge-keep-base)
;;         ("u" smerge-keep-upper)
;;         ("l" smerge-keep-lower)
;;         ("a" smerge-keep-all)
;;         ("RET" smerge-keep-current)
;;         ("C-m" smerge-keep-current)
;;         ("<" smerge-diff-base-upper)
;;         ("=" smerge-diff-upper-lower)
;;         (">" smerge-diff-base-lower)
;;         ("R" smerge-refine)
;;         ("E" smerge-ediff)
;;         ("C" smerge-combine-with-next)
;;         ("r" smerge-resolve)
;;         ("k" smerge-kill-current)
;;         ("ZZ" (lambda ()
;;                 (interactive)
;;                 (save-buffer)
;;                 (bury-buffer))
;;          "Save and bury buffer" :color blue)
;;         ("q" nil "cancel" :color blue))

;;       (bind-key (kbd "C-s-s") #'hydra-smerge/body smerge-mode-map))

;;     (with-eval-after-load 'multiple-cursors
;;       (defhydra hydra-multiple-cursors (:hint nil)
;;         "
;;      ^Up^            ^Down^        ^Other^
;; ----------------------------------------------
;; _p_   Next    _n_   Next    _l_ Edit lines
;; _P_   Skip    _N_   Skip    _a_ Mark all
;; _M-p_ Unmark  _M-n_ Unmark  _r_ Mark by regexp
;; ^ ^             ^ ^             _q_ Quit
;; "
;;         ("l" mc/edit-lines :exit t)
;;         ("a" mc/mark-all-like-this :exit t)
;;         ("n" mc/mark-next-like-this)
;;         ("N" mc/skip-to-next-like-this)
;;         ("M-n" mc/unmark-next-like-this)
;;         ("p" mc/mark-previous-like-this)
;;         ("P" mc/skip-to-previous-like-this)
;;         ("M-p" mc/unmark-previous-like-this)
;;         ("r" mc/mark-all-in-region-regexp :exit t)
;;         ("<mouse-1>" mc/add-cursor-on-click)
;;         ("<down-mouse-1>" ignore)
;;         ("<drag-mouse-1>" ignore)
;;         ("q" nil))))

;;   :bind
;;   ("C-s-v" . hydra-move/body)
;;   ("C-c w" . hydra-window/body))


;;;; Garbage Collection

(use-package gcmh
  :defer 19
  :config
  (gcmh-mode 1))


(provide 'init)

;; Local Variables:
;; flycheck-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; eval: (flycheck-mode -1)
;; End:

;;; init.el ends here
