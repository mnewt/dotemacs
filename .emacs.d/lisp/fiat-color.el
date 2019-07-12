;;; fiat-color.el --- Fiat Lux! -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton
;; Version: 0.1
;; Package-Requires: (( emacs "24.3"))
;; Homepage: https://github.com/mnewt/fiat-color
;; Keywords: theme


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

;; Fiat Lux, Fiat Nox, Fiat Color!
;; This is a system that does a few different things to try to make Emacs more
;; beautiful and colorful.

;;; Code:

(defcustom fiat-theme nil
  "The theme to load by default.

Non-nil\: Load this theme when calling `fiat-theme' without
args.

nil\: Read the last set theme from disk."
  :group 'fiat
  :type 'symbol)

(defcustom fiat-source-faces
  '(default mode-line-emphasis mode-line-highlight compilation-mode-line-fail)
  "The faces fiat-theme uses to style the mode-line."
  :group 'fiat
  :type 'list)

(defcustom fiat-status nil
  "The mode to choose when `fiat' is called."
  :group 'fiat
  :type 'symbol
  :options '(nil lux nox))

(defcustom fiat-nox-theme 'tango-dark
  "The theme activated when calling 'fiat-nox'."
  :group 'fiat
  :type 'symbol)

(defcustom fiat-lux-theme 'tango
  "The theme activated when calling `fiat-lux'."
  :group 'fiat
  :type 'symbol)

(defcustom fiat-themes (mapcar #'list (custom-available-themes))
  "Alist where car is the theme and cdr can be:

* A function to run after loading the theme.
* An alist specifying additional arguments. Possible arguments:
** hook - A function, as above.
** specs
** mouse-color

TODO: Give examples."
  :group 'fiat
  :type 'list)

(defcustom fiat-specs-common nil
  "List of default face specs to apply when a theme is activated.
This list is for specs that are common to all themes and do not
require any kind of dynamic evaluation (e.g. configuring one face
to inherit from another). For dynamic configurations, add an
alist as the cdr of the alist entry in `fiat-themes'. The
attributes specified in `fiat-themes' overrides these.

For details on face specs see `defface'."
  :group 'fiat
  :type 'list)

(defcustom fiat-config-file
  (expand-file-name "fiat-config" user-emacs-directory)
  "Fiat settings file.

It is used to load the last used settings without the need for
`desktop' or a similar external tool."
  :group 'fiat
  :type 'string)

(defvar fiat-save-variables '(fiat-theme fiat-status)
  "A list of variables to be saved to disk.")

(defvar fiat-choose-history nil
  "History list for `fiat-theme-choose'.")

(defvar fiat-before-hook nil
  "Called from `fiat-theme' before changing themes.")

(defvar fiat-after-hook nil
  "Called from `fiat-theme' after changing themes.")

(defvar fiat-selected-window (frame-selected-window)
  "Selected window.")

(defun fiat--set-selected-window ()
  "Set the variable `fiat-selected-window' appropriately.
This is used to determine whether the current window is active."
  (unless (minibuffer-window-active-p (frame-selected-window))
    (setq fiat-selected-window (frame-selected-window))
    (force-mode-line-update)))

;; Executes after a window (not a buffer) has been created, deleted, or moved.
(add-hook 'window-configuration-change-hook #'fiat--set-selected-window)

;; Executes after the window manager requests that the user's events
;; be directed to a different frame.
(advice-add 'handle-switch-frame :after #'fiat--set-selected-window)

;; Executes after the `buffer-list' changes.
(add-hook 'buffer-list-update-hook #'fiat--set-selected-window)

(defun fiat-window-active-p ()
  "Return whether the current window is active."
  (eq fiat-selected-window (selected-window)))

(defun alist-get-all (key alist &optional default testfn)
  "Return a list of *all* the elements of ALIST with matching KEY.

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
  "Get the value of NAME from OBJECT.

PROPNAME, ATTRIBUTE, and NAME are symbols which drill down to
access the individual Alist element we are after.

See `get' and `fiat-theme-attribute'."
  (let ((name (if (stringp name) (intern name) name)))
    (cl-some (lambda (e) (when (and (eq attribute (car e)) (eq name (cadr e)))
                           (cadddr e)))
             (get (car object) propname))))

(defun fiat-theme-attribute (attribute name)
  "Get the ATTRIBUTE identified by NAME from the current theme settings.)

Example usage

    \(fiat-theme-face-attribute 'default :background\)

Note that unlike `face-attribute', which gets the *current*
attribute value as displayed on the screen, this function gets
the attribute as specified in the *original* theme settings. This
is useful when you switch themes and want to calculate new faces
derived from existing ones."
  (get-attr custom-enabled-themes 'theme-settings attribute name))

(defun fiat-theme-face (face)
  "Get the FACE from the current theme.

See `fiat-theme-attribute'."
  (fiat-theme-attribute 'theme-face face))

(defun fiat-theme-face-attribute (face attribute)
  "Get the ATTRIBUTE of the FACE from the current theme.

See `fiat-theme-attribute'."
  (plist-get (face-spec-choose (fiat-theme-face face)) attribute))

(defun fiat-theme-get-value (value)
  "Get the VALUE from the current theme.

See `fiat-theme-attribute'."
  (fiat-theme-attribute 'theme-value value))

(defun fiat-search-theme-attrs (regexp)
  "Return the attributes in the current theme which match the REGEXP."
  (seq-filter (lambda (e) (string-match-p regexp (symbol-name (cadr e))))
              (get (car custom-enabled-themes) 'theme-settings)))

(defun fiat-color-blend (color1 color2 alpha)
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

;; TODO: Move this to user config.
(defun fiat-generate-theme-specs ()
  "Automatically generate theme specs the supplied faces.

See also `fiat-specs-common'. Advise or override this function
to customize further."
  (let* ((active-bg (fiat-theme-face-attribute 'default :background))
         (active-fg (fiat-theme-face-attribute 'default :foreground))
         (highlight-fg (fiat-theme-face-attribute 'highlight :foreground))
         (inactive-bg (fiat-color-blend active-bg active-fg 0.95))
         (inactive-fg (fiat-color-blend active-bg active-fg 0.4)))
    `((default ((t :background ,inactive-bg)))
      (fiat-inactive-window ((t :background ,inactive-bg)))
      (window-highlight-focused-window ((t :background ,active-bg)))
      (fringe ((t :background ,inactive-bg)))
      (vertical-border ((t :foreground ,inactive-bg)))
      (mode-line ((t :box nil
                     :underline nil
                     :background ,inactive-bg
                     :foreground ,(fiat-color-blend active-fg active-bg 0.9))))
      (mode-line-emphasis ((t :background ,(fiat-color-blend active-bg active-fg 0.7)
                              :foreground ,active-fg)))
      (mode-line-highlight ((t :background ,highlight-fg
                               :foreground ,active-bg)))
      (mode-line-buffer-id ((t :background ,(fiat-color-blend active-bg active-fg 0.2)
                               :foreground ,inactive-bg
                               :bold t)))
      (compilation-mode-line-fail ((t :inherit highlight)))
      (mode-line-inactive ((t :box nil
                              :underline nil
                              :background ,inactive-bg
                              :foreground ,inactive-fg)))
      (sp-show-pair-match-face ((t :inherit highlight
                                   :foreground nil
                                   :background nil))))))
                                   
(defun fiat--save-config ()
  "Save the configuration to disk."
  (with-temp-file fiat-config-file
    (prin1 (mapcar (lambda (v) (cons v (symbol-value v))) fiat-save-variables)
           (current-buffer))))

(defun fiat--load-config ()
  "Load the configuration from disk."
  (with-temp-buffer
    (insert-file-contents fiat-config-file)
    (goto-char (point-min))
    (cl-loop for (symbol . value) in (read (current-buffer)) do
             (set symbol value))))

;;;###autoload
(defun fiat-theme (&optional theme)
  "Switch the current Emacs theme to THEME.

Handle some housekeeping that comes with switching themes. Set
face specs specific to the theme. Having done that try to prevent
Emacs from barfing fruit salad on the screen."
  (setq theme (or theme fiat-theme (progn (fiat--load-config) fiat-theme)))
  (if theme
      (progn
        (mapc #'funcall fiat-before-hook)
        (custom-set-variables '(custom-enabled-themes nil))
        (load-theme theme t)
        (let* ((opts (alist-get theme fiat-themes)))
          (setq opts (append opts `((specs ,(fiat-generate-theme-specs)))))
          ;; Feed face specs to `custom-set-faces' in reverse because last write
          ;; wins. We do it this way so additional specs can be specified when
          ;; adding the theme to `fiat-themes'.
          (apply #'custom-set-faces
                 (append fiat-specs-common (reverse (alist-get-all 'specs opts))))
          (let-alist opts
            (set-mouse-color
             (cond
              ((boundp 'mouse-color) mouse-color)
              ((equal 'dark (frame-parameter nil 'background-mode)) "white")
              (t "black")))
            (when (boundp 'hook) (mapc #'funcall hook))))
        (setq fiat-theme theme)
        (fiat--save-config)
        (mapc #'funcall fiat-after-hook))
    (user-error "No theme specified in args, `fiat-theme', or `fiat-config-file'")))

;;;###autoload
(defun fiat-theme-choose (theme)
  "Interactively choose a THEME from `fiat-themes' and activate it."
  (interactive (list (completing-read "Load custom theme: "
                                      (mapcar #'car fiat-themes)
                                      nil t nil
                                      'fiat-choose-history)))
  (fiat-theme (intern theme)))

(defun fiat-render-mode-line (left right)
  "Return a string string concatenating LEFT and RIGHT.

Insert spaces between the two so that the string is
`window-total-width' columns wide."
  (let* ((left (apply #'concat "" left))
         (right (apply #'concat "" right))
         (space (- (window-total-width) (length left) (length right)))
         ;; Make sure space is even so right doesn't go off the screen.
         (space (if (= 1 (mod space 2)) space (1- space))))
    ;; Start with a string so left can start with nil without breaking things.
    (concat ""
            left
            ;; ?\s is a space character
            (make-string space ?\s)
            right)))

(defun fiat-ml-concat (strings &optional separator outside)
  "Concatenate the given list of STRINGS.
Optionally interpose with SEPARATOR and surround with OUTSIDE."
  (let* ((separator (or separator " "))
         (outside (when outside separator))
         (inside (string-join (cl-remove-if-not (lambda (s) (and s (> (length s) 0)))
                                                strings)
                              separator)))
    (when (> (length inside) 0)
      (concat outside inside outside))))

(defun fiat-ml-remote-hostname ()
  "Return the remote hostname for the current buffer.
Return nil if the buffer is local."
  (when (file-remote-p default-directory)
    (format " %s " (tramp-file-name-host (tramp-dissect-file-name default-directory)))))

(declare-function term-in-char-mode 'term)
(declare-function term-in-line-mode 'term)

(defun fiat-ml-term ()
  "Return the input mode for the buffer if in `term-mode'.
Return nil if otherwise."
  (when (eq major-mode 'term-mode)
    (cond
     ((term-in-char-mode) " [char] ")
     ((term-in-line-mode) " [line] "))))

(defun fiat-ml-evil ()
  "Return a mode line string indicating `evil-mode' status.
Return nil if `evil-mode' is not active."
  (when (bound-and-true-p evil-state)
    (cl-case evil-state
      (normal (propertize " NORMAL " 'face
                          `(:foreground
                            "black"
                            :background
                            ,(aref (fiat-theme-get-value 'ansi-color-names-vector) 2))))
      (insert (propertize " INSERT " 'face
                          `(:foreground
                            "white"
                            :background
                            ,(aref (fiat-theme-get-value 'ansi-color-names-vector) 4))))
      (t (propertize      "  EVIL  " 'face
                          `(:foreground
                            ,(aref (fiat-theme-get-value 'ansi-color-names-vector) 0)
                            :background
                            ,(aref (fiat-theme-get-value 'ansi-color-names-vector) 7)))))))

(defun fiat-eyebrowse-modeline ()
  "Return a mode line string with status for `eyebrowse-mode'."
  (when (bound-and-true-p eyebrowse-mode)
    (let ((current-slot (eyebrowse--get 'current-slot)))
      (string-join
       (mapcar (lambda (s)
                 (if (= s current-slot)
                     (propertize (number-to-string s) 'face 'mode-line-emphasis)
                   (number-to-string s)))
               (mapcar #'car (eyebrowse--get 'window-configs)))
       " "))))

(defun when-propertize (exp &rest properties)
  "Evaluate EXP, which should return a string or nil..
Propertize the result with the specified PROPERTIES."
  (when exp (apply #'propertize exp properties)))

(defvar fiat--old-mode-line-format nil
  "Save the old `mode-line-format'.")

;;;###autoload
(define-minor-mode fiat-mode-line-mode
  "Make a mode line whose aesthetics are inspired by vim's lightline."
  :group 'fiat
  :lighter nil
  (if fiat-mode-line-mode
      (progn
        (setq fiat--old-mode-line-format mode-line-format)
        (setq-default
         mode-line-format
         '((:eval
            (if (fiat-window-active-p)
                (fiat-render-mode-line
                 ;; left
                 (list
                  (fiat-ml-evil)
                  (when-propertize (fiat-ml-remote-hostname) 'face 'highlight)
                  (propertize (concat " " (buffer-name) " ") 'face 'mode-line-buffer-id)
                  (when (buffer-modified-p) " • ")
                  (concat " " (format-mode-line mode-name) " "))
                 ;; right
                 (list
                  (when (bound-and-true-p flycheck-mode)
                    (concat (substring (flycheck-mode-line-status-text) 1) " "))
                  (when-propertize (fiat-ml-term) 'face 'mode-line-emphasis)
                  (concat (fiat-eyebrowse-modeline) " ")
                  (when-propertize
                   (fiat-ml-concat
                    (list (when (boundp 'parinfer--mode)
                            (if (eq 'paren parinfer--mode) "🄟" "P↹"))
                          (when (buffer-narrowed-p) "⒩")
                          (when (bound-and-true-p hs-minor-mode) "⒣")
                          (when (bound-and-true-p outline-minor-mode) "⦿"))
                    " "
                    t)
                   'face 'mode-line-emphasis)
                  (propertize (format-mode-line " %l:%c  ")
                              'face 'mode-line-buffer-id)))
              (fiat-render-mode-line
               ;; left
               (list
                " "
                (buffer-name)
                " "
                (when (buffer-modified-p) " • "))
               ;; right
               (list "")))))))
    (setq-default mode-line-format fiat--old-mode-line-format)))

(defun fiat--set-os-dark-mode (darkp)
  "When DARKP is non-nil, change the OS to dark mode."
  (pcase system-type
    ('darwin
     (let ((default-directory "~") ; In case we are in a tramp session
           (mode (if darkp "true" "false")))
       (shell-command (format "osascript -e '
tell application \"System Events\"
  tell appearance preferences to set dark mode to %s
end tell'" mode))))))

;;;###autoload
(defun fiat-lux ()
  "Let the Emacs and OS themes be switched to light mode."
  (interactive)
  (setq fiat-status 'lux)
  (fiat-theme 'spacemacs-light)
  (fiat--set-os-dark-mode nil))

;;;###autoload
(defun fiat-nox ()
  "Let the Emacs and OS themes be switched to dark mode."
  (interactive)
  (setq fiat-status 'nox)
  (fiat-theme 'spacemacs-dark)
  (fiat--set-os-dark-mode t))

;;;###autoload
(defun fiat ()
  "Let the Emacs and OS themes be toggled."
  (interactive)
  (if (eq fiat-status 'nox) (fiat-lux) (fiat-nox)))
          
;; TODO: Make it work reliably. Current problems:
;; [ ] Top of window and vertical border are in `default' not `fringe' color.
;; [ ] It often changes only after a couple seconds...?
(defface fiat-inactive-window '((t :inherit fringe))
  "Face to use for default on inactive windows."
  :group 'theme)

(defun fiat-highlight-selected-window ()
  "Highlight the selected window with a different background color."
  (let ((s (selected-window))
        (m (minibuffer-selected-window)))
    (walk-windows (lambda (w)
                    (unless (eq w fiat-selected-window)
                      (with-current-buffer (window-buffer w)
                        (buffer-face-set 'fiat-inactive-window)))))
    (with-current-buffer (window-buffer s) (buffer-face-set 'default))
    (with-current-buffer (window-buffer m) (buffer-face-set 'default))))

(defun fiat-highlight-selected-window--update ()
  "Update face colors after theme change."
  (let ((inactive-bg (fiat-theme-face-attribute 'fringe :background)))
    (set-face-attribute 'default :background inactive-bg)
    (set-face-attribute 'fringe :background inactive-bg)
    (set-face-attribute 'vertical-border :background inactive-bg)))

(define-minor-mode fiat-highlight-selected-window-mode
  "Toggle highlighting of the selected window."
  :group 'theme
  :lighter nil
  (if fiat-highlight-selected-window-mode
      (progn
        (add-hook 'fiat-after-hook #'fiat-highlight-selected-window--update)
        (add-hook 'buffer-list-update-hook #'fiat-highlight-selected-window))
    (remove-hook 'buffer-list-update-hook #'fiat-highlight-selected-window)))

(provide 'fiat-color)

;;; fiat.el ends here
