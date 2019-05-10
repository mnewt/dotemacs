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

;; Desktop
(require 'desktop)
(setq desktop-dirname "~/.emacs.d")
(add-to-list 'desktop-globals-to-save 'kill-ring)
(add-to-list 'desktop-globals-to-save 'theme-current-theme)
(desktop-save-mode 1)

(defun member-list (elts list)
  "Return non-nil if one of ELTS is an element of LIST."
  (let ((result nil)
        (elts (purecopy elts)))
    (while (and (not result) elts)
      (setq result (member (pop elts) list)))
    result))

(defun upsearch-multiple (filenames &optional dir)
  "Recursively search up a directory tree for a list of FILENAMES.

Start from from DIR or `default-directory'.

Useful for finding project files like `Makefile' and `package.json'."
  (let ((dir (or dir default-directory)))
    (while (not (or (string= "/" dir)
                    (member-list filenames (directory-files dir))))
      (setq dir (file-name-directory (directory-file-name dir))))
    (unless (string= "/" dir) dir)))

(defun upsearch (filename &optional dir)
  "Recursively search up a directory tree for FILENAME."
  (let ((dir (or dir default-directory)))
    (while (not (or (string= "/" dir)
                    (member filename (directory-files dir))))
      (setq dir (file-name-directory (directory-file-name dir))))
    (unless (string= "/" dir) dir)))

(defun psync-maybe-sync ()
  "If we find a `psync_config' file then run `psync'."
  (interactive)
  (let ((default-directory (upsearch "psync_config")))
    (when (file-exists-p (expand-file-name "psync_config"))
      (if (= 0 (shell-command-exit-code "psync"))
          (message "psync in directory %s finished." default-directory)
        (message "psync in directory %s failed." default-directory)))))

(add-hook 'after-save-hook #'psync-maybe-sync)

(bind-key "C-x M-s" #'psync-maybe-sync)

(provide 'm-persist)

;;; m-persistence.el ends here
