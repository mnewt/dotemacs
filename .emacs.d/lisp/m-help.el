;;; m-help.el --- Help and Documentation -*- lexical-binding: t -*-

;;; Commentary:

;; Help and Documentation lookup

;;; Code:

(setq suggest-key-bindings 5
      ;; Select help window so it's easy to quit it with `q'
      help-window-select t)

(use-package epkg
  :commands
  (epkg epkg-describe-package epkg-list-packages))

(defun update-packages ()
  "Use straight.el to update all packages."
  (interactive)
  (straight-normalize-all)
  (straight-fetch-all)
  (straight-merge-all))

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
(mapc (lambda (m) (add-hook m #'turn-on-eldoc-mode))
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

(defun dash-docs-update-all-docsets ()
  "Update all docsets."
  (interactive)
  (mapc (lambda (d) (when (memq d (dash-docs-official-docsets))
                      (dash-docs-install-docset d)))
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

(provide 'm-help)

;;; m-help.el ends here
