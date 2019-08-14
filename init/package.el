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

;;;; use-package-list -- track defined packages

(add-to-list 'use-package-keywords :list)

(add-to-list 'use-package-defaults
             '(:list t t))

(defvar use-package-list nil
  "Packages defined by `use-package'.")

(defun use-package-normalize/:list (_name _keyword _args)
  "Serves no function; here only as boilerplate."
  (list t))

(defun use-package-handler/:list (name _keyword _ensure rest state)
  "Add the package NAME to the list."
  (let* ((body (use-package-process-keywords name rest state)))
    (add-to-list 'use-package-list name)
    body))

;; TODO Develop a better way to ensure only currently configured packages are
;; installed. Use `use-package-list'.
;; See https://yoo2080.wordpress.com/2014/05/16/how-to-list-emacs-package-dependencies/
(defun package-delete-all ()
  "Delete all packages in `package-user-dir'.

We do this to get rid of any stale packages and force a reinstall
on the next startup."
  (interactive)
  (shell-command (concat "rm -rf " package-user-dir)))

(defvar package-dependencies-alist nil
  "List of packages and their dependencies.")

(defun package-refresh-dependencies-alist ()
  "Refresh `package-dependencies-alist'."
  (setq package-dependencies-alist
        (cl-loop for pkg in package-activated-list
                 for pkg-vec = (cadr (assq pkg package-alist))
                 when pkg-vec
                 collect (cons pkg
                               (cl-loop for req in (package-desc-reqs pkg-vec)
                                        for req-name = (car req)
                                        when (memq req-name package-activated-list)
                                        collect req-name))))
  package-dependencies-alist)

(defun find-duplicates (list)
  "Get the duplicate elements from LIST."
  (cl-loop for (item . count) in
           (let ((counts '())
                 place)
             (dolist (el list)
               (setq place (assoc el counts))
               (if place
                   (cl-incf (cdr place))
                 (push (cons el 1) counts)))
             counts)
           if (> count 1)
           collect item))

(defun package-delete-unused ()
  "Delete unused packages."
  (interactive)
  (let* ((default-directory package-user-dir)
         ;; Could do this from `package-alist' / package-desc but what we really
         ;; care about is what is on disk, so go straight to it.
         installed-package-alist duplicates)
    (dolist (dir (file-expand-wildcards "*-*"))
      (when (file-directory-p dir)
        (push (cons (intern (replace-regexp-in-string "-[0-9\\.]+\\'" "" dir))
                    dir)
              installed-package-alist)))
    (setq duplicates
          (mapcar (lambda (dup) (sort (seq-filter
                                       (lambda (e) (equal dup (car e)))
                                       installed-package-alist)
                                      (lambda (a b)
                                        (string-greaterp (cdr a) (cdr b)))))
                  (find-duplicates (mapcar #'car installed-package-alist))))
    ;;     (dolist (dup duplicates))))
    ;; \      (pp (concat "rm -rf " (string-join old-files " "))))))
    ;; TODO: Delete all but newest duplicate.

    (pp (car duplicates))))

(defun byte-compile-directory (directory)
  "Byte compile all Emacs Lisp files in DIRECTORY."
  (interactive (read-directory-name "Byte compile directory: "))
  (let ((default-directory directory))
    (dolist (filename (file-expand-wildcards "*.el"))
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
