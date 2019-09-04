;;; bind.el --- base key bindings -*- lexical-binding: t -*-

;;; Commentary:

;; Define base key bindings

;;; Code:

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

(provide 'm-bind)

;;; bind.el ends here
