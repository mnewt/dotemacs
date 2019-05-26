;;; m-help.el --- Help and Documentation -*- lexical-binding: t -*-

;;; Commentary:

;; Help and Documentation lookup

;;; Code:

(use-package simple
  :straight (:type built-in)
  :custom
  (suggest-key-bindings 5))

(use-package epkg
  :commands
  (epkg epkg-describe-package epkg-list-packages))

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
 ("C-h C-i" . elisp-index-search)
 ("C-h M-i" . info-apropos)
 :map Info-mode-map
 ("j" . next-line)
 ("k" . previous-line)
 ("J" . next-line-4)
 ("K" . previous-line-4)
 :map  help-mode-map)

(seq-doseq (m '(Info-mode-map help-mode-map))
  (bind-keys :map (symbol-value m)
             ("j" . next-line)
             ("k" . previous-line)
             ("J" . next-line-4)
             ("K" . previous-line-4)))

(use-package shr
  :custom
  (shr-color-visible-luminance-min 60)
  (shr-color-visible-distance-min 5)
  (shr-use-colors nil))

(use-package eldoc
  :hook
  (after-init . global-eldoc-mode))

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

(defun tramp-aware-woman (man-page-path)
  "Open a remote man page at MAN-PAGE-PATH via TRAMP."
  (interactive)
  (let ((dir (eshell/pwd)))
    (woman-find-file
     (if (file-remote-p dir)
         (let ((vec (tramp-dissect-file-name dir)))
           (tramp-make-tramp-file-name
            (tramp-file-name-method vec)
            (tramp-file-name-user vec)
            (tramp-file-name-host vec)
            man-page-path))
       man-page-path))))

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

(defvar dash-docs-docsets-path "~/.config/docsets"
  "Local path to save docsets.")

(defun dash-docs-installed-docsets ()
  "Return a list of the currently installed docsets."
  (mapcar (lambda (f) (string-trim-right f ".docset"))
          (directory-files dash-docs-docsets-path nil "[^.]*\.docset")))

(defun dash-docs-update-all-docsets ()
  "Update all docsets."
  (interactive)
  (seq-doseq (d (dash-docs-installed-docsets))
    (when (memq d (dash-docs-official-docsets))
      (dash-docs-install-docset d))))

(use-package counsel-dash
  ;; :ensure-system-package sqlite3
  :custom
  (dash-docs-browser-func (if (fboundp #'w3m) #'w3m #'eww))
  (dash-docs-common-docsets (dash-docs-installed-docsets))
  :commands
  (counsel-dash counsel-dash-at-point
                counsel-dash-install-docset
                dash-docs-official-docsets)
  :bind
  ("M-s-l" . counsel-dash)
  ("C-h C-d" . counsel-dash)
  ("M-s-." . counsel-dash-at-point))

(provide 'm-help)

;;; m-help.el ends here
