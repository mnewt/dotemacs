;;; m-appearance.el --- Appearance Related Configuration -*- lexical-binding: t -*-

;; Author: Matthew Newton


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

;; commentary

;;; Code:

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

(use-package window-highlight
  :if (>= emacs-major-version 27)
  :straight
  (:type git :host github :repo "dcolascione/emacs-window-highlight")
  :hook
  (after-init . window-highlight-mode))

(use-package darkroom
  :bind
  ("C-c C-d" . darkroom-mode)
  :commands
  (darkroom-mode))

(bind-key "C-c C-t" #'theme-choose)

(provide 'm-appearance)

;;; m-appearance.el ends here
