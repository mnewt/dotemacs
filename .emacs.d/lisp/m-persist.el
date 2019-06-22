;;; m-persist.el --- Persist State Between Emacs Sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Persist Emacs session data.

;;; Code:

;; Store all backup and autosave files in their own directory since it is bad to
;; clutter project directories.
(with-eval-after-load 'files
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
        undo-limit 5242880))

(use-package savehist
  :defer 1
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
  :config
  (savehist-mode))

(use-package saveplace
  :defer 1
  :config
  (save-place-mode))

(use-package recentf
  :defer 1
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'never)
  :config
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))

  (defun recentf-save-list-silent ()
    (let ((message-log-max nil))
      (if (fboundp 'shut-up)
          (shut-up (recentf-save-list))
        (recentf-save-list))))

  (defun recentf-cleanup-silent ()
    (let ((message-log-max nil))
      (if (fboundp 'shut-up)
          (shut-up (recentf-cleanup))
        (recentf-cleanup))))

  ;; (shut-up (recentf-mode))
  (recentf-mode)
  :hook
  (focus-out-hook . (recentf-save-list-silent recentf-cleanup-silent))
  (dired-mode . recentf-add-dired-directory))

(use-package autorevert
  :defer 2
  :custom
  ;; Work in Dired.
  (global-auto-revert-non-file-buffers t)
  ;; Don't print auto revert messages.
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode))

;; (use-package desktop
;;   :demand t
;;   :custom
;;   (desktop-dirname "~/.emacs.d")
;;   :config
;;   (dolist (v '(kill-ring read-expression-history theme-current-theme))
;;     (add-to-list 'desktop-globals-to-save v))
;;   (desktop-save-mode))

(provide 'm-persist)

;;; m-persist.el ends here
