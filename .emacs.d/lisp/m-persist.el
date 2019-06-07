;;; m-persist.el --- Persist State Between Emacs Sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Persist Emacs session data.

;;; Code:

(use-package savehist
  :custom
  (savehist-autosave-interval 60)
  (history-length 200)
  (history-delete-duplicates t)
  (savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring
                                   file-name-history
                                   magit-read-rev-history
                                   read-expression-history
                                   command-history
                                   extended-command-history
                                   ivy-history))
  :hook
  (after-init-hook . savehist-mode))

(use-package saveplace
  :hook
  (after-init . save-place-mode))

(defun recentd-track-opened-file ()
  "Insert the name of the directory just opened into the recent list."
  (require 'recentf)
  (and (derived-mode-p 'dired-mode) default-directory
       (recentf-add-file default-directory))
  ;; Must return nil because it is run from `write-file-functions'.
  nil)

(defun recentf-save-list-silent ()
  (let ((message-log-max nil))
    (if (fboundp 'shut-up)
        (shut-up (recentf-save-list))
      (recentf-save-list))))

(defun recentf-cleanup-silent ()
  (let ((message-log-max nil))
    (if shutup-p
        (shut-up (recentf-cleanup))
      (recentf-cleanup))))

(use-package recentf
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'never)
  :hook
  (focus-out-hook . (recentf-save-list-silent recentf-cleanup-silent))
  (after-init . recentf-mode)
  (dired-after-readin . recentd-track-opened-file))

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
      ;; Increase undo limit to 5MB per buffer.
      undo-limit 5242880)

(use-package autorevert
  :custom
  ;; Work in Dired.
  (global-auto-revert-non-file-buffers t)
  ;; Don't print auto revert messages.
  (auto-revert-verbose nil)
  :hook
  (after-init . global-auto-revert-mode))

(use-package desktop
  :custom
  (desktop-dirname "~/.emacs.d")
  (desktop-restore-eager 3)
  :config
  (dolist (v '(kill-ring read-expression-history theme-current-theme))
    (add-to-list 'desktop-globals-to-save v))
  (desktop-save-mode))

(provide 'm-persist)

;;; m-persist.el ends here
