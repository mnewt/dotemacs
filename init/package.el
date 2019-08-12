;;; package.el --- Package management -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs Package Management

;;; Code:

;;;; use-package

(defvar elisp-directory "~/.emacs.d/lisp"
  "Local elisp configuration files go here.")

(setq package-enable-at-startup nil
      package-user-dir "~/.emacs.d/packages/"
      package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/"))
      package-archive-priorities '(("org" . 30)
                                   ("melpa" . 20)
                                   ("elpa" . 10))
      custom-file "~/.emacs.d/custom.el")

;; https://github.com/nilcons/emacs-use-package-fast
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        ;; (require 'package)
        (package-initialize)
        ;; use-package customizations
        (custom-set-variables
         '(use-package-always-ensure t)
         '(use-package-always-defer t)
         '(use-package-enable-imenu-support t)
         '(use-package-hook-name-suffix nil))
        ;; Install use-package if not installed yet.
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        ;; (require 'use-package)
        (setq use-package-always-ensure t)
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse (apply #'nconc
                           ;; Only keep package.el provided loadpaths.
                           (mapcar #'(lambda (path)
                                       (if (string-prefix-p package-user-dir-real path)
                                           (list path)
                                         nil))
                                   load-path))))))

(use-package benchmark-init
  :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'emacs-startup-hook 'benchmark-init/deactivate))

(add-to-list 'load-path elisp-directory)

(use-package use-package-git :demand t :ensure nil)

(use-package use-package-ensure-system-package :demand t)

;; TODO Develop a better way to ensure only currently configured packages are
;; installed.
(defun package-delete-all ()
  "Delete all packages in `package-user-dir'.

We do this to get rid of any stale packages and force a reinstall
on the next startup.")

(defun git-ls-files (&optional directory)
  "Return a list of the files from `git ls-files DIRECTORY'."
  (split-string (shell-command-to-string
                 (concat "git ls-files " (or directory default-directory)))))

(defun byte-compile-directory (directory)
  "Byte compile all Emacs Lisp files in DIRECTORY."
  (interactive (read-directory-name "Byte compile directory: "))
  (let ((default-directory directory))
    (dolist (filename (git-ls-files))
      (byte-compile-file filename))))

(defun byte-compile-init-files ()
  "Byte compile all Emacs init files."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (byte-compile-file "init.el"))
  (byte-compile-directory "~/.emacs.d/init")
  (byte-compile-directory "~/.emacs.d/lisp"))

(defun emacs-startup-message ()
  "Display a message after Emacs startup."
  (defconst emacs-load-time
    (float-time (time-subtract (current-time) emacs-start-time)))

  (message "Emacs loaded %d packages in %.1f seconds."
           (+ (length package-activated-list) (length use-package-git--packages))
           emacs-load-time))

(add-hook 'emacs-startup-hook #'emacs-startup-message)

(provide 'm-package)

;;; package.el ends here
