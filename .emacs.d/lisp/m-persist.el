;;; m-persistence.el --- Persist State Between Emacs Sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Persist Emacs session data.

;;; Code:

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
      ;; Increase undo limit to 5MB per buffer.
      undo-limit 5242880)

;; Whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on disk.
(global-auto-revert-mode 1)
;; Auto refresh dired
(setq global-auto-revert-non-file-buffers t
      ;; Don't print auto revert messages.
      auto-revert-verbose nil)

(use-package desktop
  :custom
  (desktop-dirname "~/.emacs.d")
  :config
  (add-to-list 'desktop-globals-to-save 'kill-ring)
  (add-to-list 'desktop-globals-to-save 'theme-current-theme)
  (desktop-save-mode 1))

(provide 'm-persist)

;;; m-persistence.el ends here
