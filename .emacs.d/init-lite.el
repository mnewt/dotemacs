;;; Init.el --- Emacs init file --- -*- lexical-binding: t -*-

;;; Commentary:
;; It's an Emacs init file for deployment as a single file to servers that need
;; to be managed. The idea is to have a comfortable editing environment with no
;; external packages installed.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Top Level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set Garbage Collection threshold to a larger value, run GC on idle.
(setq gc-cons-threshold (* 5 gc-cons-threshold))

(run-with-idle-timer 5 nil
                     (lambda ()
                       (run-with-idle-timer 20 t (lambda () (garbage-collect)))))

(setq load-prefer-newer t)

(with-eval-after-load 'gnutls
  (setq gnutls-verify-error t))

(with-eval-after-load 'nsm
  (setq network-security-level 'high))

;; Disable package.el initialization.
(setq package-enable-at-startup nil
      ;; don't add `custom-set-variables' block
      package--init-file-ensured t)

(defmacro bind-keys (&rest bindings)
  "Define multiple key BINDINGS for a given KEYMAP at once.

This is a small subset of the real `bind-keys' macro implemented
so we don't have to load any external libraries."
  (let ((map (current-global-map))
        defs)
    (while bindings
      (let ((x (pop bindings)))
        (if (and (keywordp x) (eq x :map))
            (setq map (pop bindings))
          (setq defs (cons `(define-key ,map ,(kbd (car x)) ',(cdr x)) defs)))))
    `(progn ,@defs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure the frame
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

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

;; Ripped from `doom-themes'
(defun doom-themes-visual-bell-fn ()
  "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
  (unless doom-themes--bell-p
    (let ((old-remap (copy-alist face-remapping-alist)))
      (setq doom-themes--bell-p t)
      (setq face-remapping-alist
            (append (delete (assq 'mode-line face-remapping-alist)
                            face-remapping-alist)
                    '((mode-line doom-modeline-error))))
      (force-mode-line-update)
      (run-with-timer 0.15 nil
                      (lambda (remap buf)
                        (with-current-buffer buf
                          (when (assq 'mode-line face-remapping-alist)
                            (setq face-remapping-alist remap
                                  doom-themes--bell-p nil))
                          (force-mode-line-update)))
                      old-remap
                      (current-buffer)))))

(setq ring-bell-function #'doom-themes-visual-bell-fn
      visible-bell t)

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

(defun when-propertize (exp &rest properties)
  "Propertize the result of body or return `nil'."
  (when exp (apply #'propertize exp properties)))

(defun s-pad-left (len padding s)
  "If S is shorter than LEN, pad it with PADDING on the left."
  (declare (pure t) (side-effect-free t))
  (let ((extra (max 0 (- len (length s)))))
    (concat (make-string extra (string-to-char padding))
            s)))

(setq-default
 mode-line-format
 '((:eval
    (if (theme-window-active-p)
        (theme-render-mode-line
         ;; left
         (list
          (when-propertize (theme-ml-remote-hostname) 'face 'mode-line-highlight)
          (propertize (concat " " (buffer-name) " ") 'face 'mode-line-buffer-id)
          (when (buffer-modified-p) " • "))
         ;; right
         (list
          (when-propertize (theme-ml-term-mode) 'face 'mode-line-emphasis)
          ;; Some modes, e.g. `dired+', set `mode-name' to something fancy that
          ;; must be evaluated with `format-mode-line'.
          (concat " "(format-mode-line mode-name) " ")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show line in the original buffer from occur mode
(setq list-matching-lines-jump-to-current-line t)

(add-hook 'after-init-hook #'winner-mode)
(global-set-key (kbd "C-c [") #'winner-undo)
(global-set-key (kbd "C-c [") #'winner-redo)

(global-set-key (kbd "C-c <up>") #'buf-move-up)
(global-set-key (kbd "C-c <down>") #'buf-move-down)
(global-set-key (kbd "C-c <left>") #'buf-move-left)
(global-set-key (kbd "C-c <right>") #'buf-move-right)

(global-set-key (kbd "M-o") #'other-window)

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

(global-set-key (kbd "C-<tab>") #'hs-toggle-hiding)
(add-hook 'hs-minor-mode '# hs-hide-all)

(global-set-key (kbd "s-o") #'find-file)
(global-set-key (kbd "s-O") #'find-file-other-window)
(global-set-key (kbd "s-s") #'save-buffer)
(global-set-key (kbd "s-S") #'write-file)
(global-set-key (kbd "s-q") #'save-buffers-kill-emacs)
(global-set-key (kbd "s-z") #'undo)
(global-set-key (kbd "C-z") #'undo)
(global-set-key (kbd "s-x") #'kill-line-or-region)
(global-set-key (kbd "s-c") #'copy-line-or-region)
(global-set-key (kbd "s-v") #'clipboard-yank-and-indent)
(global-set-key (kbd "s-a") #'mark-whole-buffer)
(global-set-key (kbd "s-g") #'isearch-repeat-forward)
(global-set-key (kbd "s-G") #'isearch-repeat-backward)
(global-set-key (kbd "C-S-s") #'isearch-forward-symbol-at-point)
(global-set-key (kbd "s-l") #'select-current-line)
(global-set-key (kbd "C-S-L") #'select-current-line)
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "s-b") #'switch-to-buffer)
(global-set-key (kbd "s-B") #'switch-to-buffer-other-window)
(global-set-key (kbd "s-\`") #'other-frame)
(global-set-key (kbd "C-\`") #'other-frame)
(global-set-key (kbd "s-w") #'delete-window)
(global-set-key (kbd "s-W") #'delete-other-windows)
(global-set-key (kbd "s-C-w") #'delete-frame)
(global-set-key (kbd "s-/") #'comment-toggle)
(global-set-key (kbd "s-h") #'ns-do-hide-emacs)
(global-set-key (kbd "s-H") #'ns-do-hide-others)

(global-set-key (kbd "C-c U") #'revert-buffer)
(global-set-key (kbd "C-c i") #'os-reveal-file)
(global-set-key (kbd "s-<return>") #'eval-last-sexp)
(global-set-key (kbd "s-RET") #'eval-last-sexp)
(global-set-key (kbd "s-n") #'new-scratch-buffer)
(global-set-key (kbd "s-N") #'new-scratch-buffer-other-window)
(global-set-key (kbd "C-c C-n") #'new-scratch-buffer)
(global-set-key (kbd "C-c M-n") #'new-scratch-buffer-other-window)
(global-set-key (kbd "C-S-p") #'previous-line-4)
(global-set-key (kbd "C-S-n") #'next-line-4)
(global-set-key (kbd "H-p") #'"\C-u1\M-v")
(global-set-key (kbd "H-n") #'"\C-u1\C-v")

;; Quick switch buffers
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "s-}") #'next-buffer)
(global-set-key (kbd "C-c }") #'next-buffer)
(global-set-key (kbd "s-{") #'previous-buffer)
(global-set-key (kbd "C-c {") #'previous-buffer)
(global-set-key (kbd "C-s-j") #'switch-to-buffer-by-mode)
(global-set-key (kbd "C-c M-j") #'switch-to-buffer-by-mode)

;; windmove
(global-set-key (kbd "H-a") #'windmove-left)
(global-set-key (kbd "H-h") #'windmove-left)
(global-set-key (kbd "H-d") #'windmove-right)
(global-set-key (kbd "H-l") #'windmove-right)
(global-set-key (kbd "H-w") #'windmove-up)
(global-set-key (kbd "H-j") #'windmove-up)
(global-set-key (kbd "H-s") #'windmove-down)
(global-set-key (kbd "H-k") #'windmove-down)
(global-set-key (kbd "M-]") #'windmove-right)
(global-set-key (kbd "M-[") #'windmove-left)

;; Resize windows
(global-set-key (kbd "M-s-<up>") #'shrink-window)
(global-set-key (kbd "M-s-<down>") #'enlarge-window)
(global-set-key (kbd "M-s-<left>") #'shrink-window-horizontally)
(global-set-key (kbd "M-s-<right>") #'enlarge-window-horizontally)

;; Navigate with mark
(global-set-key (kbd "M-s-,") #'pop-to-mark-command)
(global-set-key (kbd "C-c ,") #'pop-to-mark-command)
(global-set-key (kbd "s-,") #'pop-global-mark)
(global-set-key (kbd "C-c C-,") #'pop-global-mark)

;; Kill buffer and window at the same time.
(global-set-key (kbd "M-s-w") #'kill-buffer-and-window)
(global-set-key (kbd "M-s-W") #'kill-other-buffer-and-window)

;; Tags
(global-set-key (kbd "s-R") #'xref-find-definitions-other-window)
(global-set-key (kbd "C-c M-r") #'xref-find-definitions-other-window)

(global-set-key (kbd "C-c C-f") #'find-file-at-point-with-line)

;; Init
(global-set-key (kbd "C-c I") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(define-key 'ctl-x-4-map (kbd"t") #'toggle-window-split)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help and Documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq suggest-key-bindings 5
      ;; Select help window so it's easy to quit it with `q'
      help-window-select t)

(global-set-key (kbd "C-h C-i") #'#'elisp-index-search)
(global-set-key (kbd "C-h M-i") #'#'info-apropos)

(global-eldoc-mode)

;; ELDoc
(mapc (lambda (m) (add-hook m #'turn-on-eldoc-mode))
      '(emacs-lisp-mode-hook
        lisp-interaction-mode-hook
        ielm-mode-hook))

;; Make the manpage the current buffer in the other window
(setq Man-notify-method 'aggressive)

(set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
(set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)

(global-set-key (kbd "C-h M-m") #'man)

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

(setq sh-basic-offset tab-width
      sh-indentation tab-width)

;; Tell `executable-set-magic' to insert #!/usr/bin/env interpreter
(executable-prefix-env t)

;; Make a shell script executable automatically on save
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'before-save  #'maybe-reset-major-mode)

(define-key 'sh-mode-map "s-<ret>" #'eshell-send-current-line)

;; dw (https://gitlab.com/mnewt/dw)
(add-to-list 'auto-mode-alist '("\\DWfile.*\\'" . sh-mode))

;; Automatically indent after RET
(electric-indent-mode +1)

(defun auto-fill-mode-setup ()
  "Automatically fill comments. Wraps on `fill-column' columns."
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'prog-mode-hook #'auto-fill-mode-setup)

;; http://whattheemacsd.com/key-bindings.el-03.html
(defun delete-indentation-forward ()
  "Like `delete-indentation', but in the opposite direction.
Bring the line below point up to the current line."
  (interactive)
  (join-line -1))

(defun indent-buffer-or-region (beg end &optional arg)
  "Indent the region if one is active, otherwise format the buffer.
Some modes have special treatment."
  (interactive "rP")
  (when (sp-point-in-string-or-comment)
    (fill-paragraph arg))
  (call-interactively #'crux-cleanup-buffer-or-region)
  (if (use-region-p)
      (progn
        (indent-region beg end)
        (message "Region indented."))
    (progn
      (format-all-buffer)
      (message "Buffer formatted."))))

(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-buffer-or-region (region-beginning) (region-end)))
  (message "Defun indented."))

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

(bind-keys
 ("C-x r E" . expression-to-register)
 ("C-x r e" . eval-register)
 ("C-M-\\" . indent-buffer-or-region)
 ("C-\\" . indent-defun)
 ("C-^" . delete-indentation-forward)
 ("s-C" . copy-line-or-region-to-other-window)
 ("s-X" . move-line-or-region-to-other-window)
 ;; Replace `delete-horizontal-space' with the more useful `cycle-spacing'.
 ("M-\\" . cycle-spacing)
 ;; Continue comment on next line (default binding is "C-M-j")
 ("M-RET" . indent-new-comment-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; visual-line-mode
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

;;;; Org

(require 'org)
(require 'org-capture)

(defun search-org-files ()
  "Search ~/org using `counsel-rg'."
  (interactive)
  (counsel-rg nil org-directory))

(defun org-todo-todo ()
  "Create or update Org todo entry to TODO status."
  (interactive)
  (org-todo "TODO"))

(defun org-todo-to-int (todo)
  "Get the number of the TODO based on its status."
  (first (-non-nil
          (mapcar (lambda (keywords)
                    (let ((todo-seq
                           (mapcar (lambda (x) (first (split-string  x "(")))
                                   (rest keywords))))
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

(setq ;; Clean view
      org-startup-indented t
      ;; Smart C-a/e
      org-special-ctrl-a/e t
      ;; Smart C-k
      org-special-ctrl-k t
      ;; Insert a row in tables
      org-special-ctrl-o t
      ;; Tab in source blocks should act like in major mode
      org-src-tab-acts-natively t
      ;; Code highlighting in code blocks
      org-src-fontify-natively t
      ;; Customize todo keywords
      ;; (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WIP(w)" "|" "DONE(d!)")))
      ;; (org-todo-keyword-faces '(("TODO" (:foreground "orange" :weight bold))
      ;;                           ("NEXT" (:foreground "red" :weight bold))
      ;;                           ("WIP" (:foreground "green" :weight bold))
      ;;                           ("DONE" (:foreground "gray"))))
      org-agenda-files '(org-directory
                         (expand-file-name "TODO.org" org-directory)))

(add-to-list
 'org-capture-templates
 '("m"
   "TODO respond to email"
   entry
   (file (expand-file-name "TODO.org" org-directory))
   "* TODO %^{Description}\n%A\n%?\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("Cask\\'" emacs-lisp-mode))

(defun eval-last-sexp-other-window (arg)
  "Run `eval-last-sexp' with ARG in the other window."
  (interactive "P")
  (save-window-excursion
    (other-window 1)
    (eval-last-sexp arg)))

(defun expression-to-register (register)
  "Interactively store an Emacs Lisp expression in a REGISTER.
If region is active, store that. Otherwise, store the sexp at
  point."
  (interactive (list (register-read-with-preview "Copy expression to register: ")))
  (set-register register
                (if (region-active-p)
                    (buffer-substring (mark) (point))
                  (destructuring-bind (start . end) (bounds-of-thing-at-point 'sexp)
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

(my-bind-keys
 :map lisp-mode-shared-map
 ("s-<return>" . eval-last-sexp)
 ("C-s-<return>" . eval-last-sexp-other-window)
 ("C-c C-k" . eval-buffer)
 ("C-x C-r" . eval-region)
 ("C-x M-e" . pp-macroexpand-last-sexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Automate communication with services, such as nicserv
(with-eval-after-load 'erc
  (require 'erc-services)
  (erc-services-mode 1))

;; dw (https://gitlab.com/mnewt/dw)
(add-to-list 'auto-mode-alist '("\\DWfile.*\\'" . sh-mode))

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
