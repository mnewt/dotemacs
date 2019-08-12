;;; appear.el --- Appearance Related Configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Set up visual UI and theme stuff.

;;; Code:

(custom-set-variables '(inhibit-splash-screen t))

;; Beeping is REALLY NOT OK
(setq visible-bell t
      ;; Show keystrokes right away, don't show the message in the scratch
      ;; buffer.
      echo-keystrokes 0.01)

;; Configure the frame
(if window-system
    (progn
      (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
      (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
      (when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
      (setq frame-resize-pixelwise t
            ;; We don't set a frame title because Emacs on macOS renders the frame title
            ;; face terribly. No rendering is better than terrible rendering.
            frame-title-format nil
            ;; No icon in the titlebar
            ns-use-proxy-icon nil
            ;; Smoother and nicer scrolling
            scroll-margin 6
            scroll-step 1
            scroll-conservatively 10000
            scroll-preserve-screen-position 1
            auto-window-vscroll nil)

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
        "My default font to use for fixed pitch applications.")
      
      (defvar m-variable-pitch-font
        (some-font '("Avenir-17" "Calibri" "Helvetica Neue" "Helvetica" "Georgia-15"))
        "My default font to use for variable pitch applications.")

      (dolist (face '(default fixed-pitch))
        (set-face-font face m-fixed-pitch-font))
      
      (set-face-font 'variable-pitch m-variable-pitch-font)

      (add-hook 'text-mode-hook 'variable-pitch-mode)

      ;; Wrap text at the end of a line like a word processor.
      (add-hook 'text-mode-hook #'turn-on-visual-line-mode)

      ;; No GUI dialogs
      (setq use-dialog-box nil)

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

      (use-package window-highlight
        :defer 0.1
        :if window-system
        :git "https://github.com/dcolascione/emacs-window-highlight"
        :commands
        window-highlight-mode
        :config
        (window-highlight-mode))

      (use-package spacemacs-theme
        :defer 0.1
        :custom
        (spacemacs-theme-comment-bg nil)))

  ;; No GUI
  (menu-bar-mode -1))

(use-package hl-line
  :defer 1
  :commands
  global-hl-line-mode
  :config
  (global-hl-line-mode))

(use-package fiat-color
  :demand t
  :after window-highlight
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
  ("C-c C-t" . fiat-theme-choose)
  ("C-M-s-t" . fiat)
  ("M-m t t" . fiat))

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
     ("NOTE" . "orange")
     ("KLUDGE" . "orange")
     ("HACK" . "orange")
     ("TEMP" . "orange")
     ("XXX+" . "orange")
     ("NEXT" . "lime green")))
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

(provide 'm-appearance)

;;; appear.el ends here
