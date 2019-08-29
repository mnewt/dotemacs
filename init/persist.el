;;; persist.el --- Persist State Between Emacs Sessions -*- lexical-binding: t -*-

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
        ;; Don't create `#file-name' lockfiles in $PWD. Lockfiles are useful but it
        ;; generates too much activity from tools watching for changes during
        ;; development.
        create-lockfiles nil
        ;; Increase undo limit to 5MB per buffer.
        undo-limit 5242880))

(use-package saveplace
  :defer 1
  :commands
  save-place-mode
  :config
  (save-place-mode))

(use-package recentf
  :defer 1
  :commands
  recentf-save-list
  recentf-cleanup
  recentf-mode
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

  (defun grep-recent-files (filepattern pattern)
    (interactive "sFiles regexp: \nsSearch regexp: ")
    (let ((files (if filepattern
                     (cl-remove-if-not (lambda (item) (string-match filepattern item))
                                       recentf-list)
                   recentf-list))
          (limit 50)
          (grep-use-null-device nil))
      (if (> (length files) limit)
          (seq-subseq files 0 limit))

      (let* ((tempfile (make-temp-file "emacs"))
             (orig compilation-finish-functions))
        (add-to-list 'compilation-finish-functions
                     (lambda (_buf _result)
                       (setq font-lock-keywords-case-fold-search t)
                       (highlight-regexp pattern 'hi-yellow)
                       (delete-file tempfile)
                       (setq compilation-finish-functions orig)))

        (write-region  (mapconcat 'identity files (char-to-string 0))
                       nil tempfile)

        (grep (format "%s %s | xargs -0 grep -n -i \"%s\" "
                      (if (eq system-type 'windows-nt)
                          "type"
                        "cat")

                      (if (eq system-type 'windows-nt)
                          (replace-regexp-in-string "/" "\\\\" tempfile)
                        tempfile)

                      pattern)))))

  (recentf-mode)
  :hook
  (dired-mode-hook . recentf-add-dired-directory))

(use-package autorevert
  :defer 2
  :custom
  ;; Work in Dired.
  (global-auto-revert-non-file-buffers t)
  ;; Don't print auto revert messages.
  (auto-revert-verbose nil)
  :commands
  global-auto-revert-mode
  :config
  (global-auto-revert-mode))

(use-package psession
  :demand t
  :custom
  (psession-object-to-save-alist
   '((command-history . "command-history.el")
     (extended-command-history . "extended-command-history.el")
     (regexp-search-ring . "regexp-search-ring.el")
     (search-ring . "search-ring.el")
     (file-name-history . "file-name-history.el")
     (kill-ring . "kill-ring.el")
     (kill-ring-yank-pointer . "kill-ring-yank-pointer.el")
     (register-alist . "register-alist.el")
     (comint-input-ring . "comint-input-ring.el")
     (file-name-history . "file-name-history.el")
     (eshell-history-ring . "eshell-history-ring.el")
     (minibuffer-history-variable . "minibuffer-history-variable.el")
     (regexp-history . "regexp-history.el")
     (ivy-history . "ivy-history.el")
     (psession--save-buffers-alist . "psession-save-buffers-alist.el")
     (eyebrowse-last-window-config . "eyebrowse-last-window-config.el")))
  :config
  (psession-mode)
  (psession-autosave-mode)
  (psession-savehist-mode))

(provide 'm-persist)

;;; persist.el ends here
