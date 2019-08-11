;;; m-package.el --- Package management -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs Package Management

;;; Code:

;;;; package.el

(setq package-enable-at-startup nil
      custom-file "~/.emacs.d/custom.el")

;;;; Quelpa

;; (unless (require 'quelpa nil t)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://framagit.org/steckerhalter/quelpa/raw/master/bootstrap.el")
;;     (eval-buffer)))

;;;; straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(custom-set-variables
 '(straight-use-package-by-default t)
 '(straight-cache-autoloads t)
 '(use-package-enable-imenu-support t)
 '(use-package-hook-name-suffix nil))

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (require 'use-package)
  

;;;; leaf

;; (prog1 "prepare leaf"
;;   (prog1 "package"
;;     (custom-set-variables
;;      '(package-archives '(("org"   . "https://orgmode.org/elpa/")
;;                           ("melpa" . "https://melpa.org/packages/")
;;                           ("gnu"   . "https://elpa.gnu.org/packages/"))))
;;     (package-initialize))

;;   (prog1 "leaf"
;;     (unless (package-installed-p 'leaf)
;;       (unless (assoc 'leaf package-archive-contents)
;;         (package-refresh-contents))
;;       (condition-case err
;;           (package-install 'leaf)
;;         (error
;;          (package-refresh-contents)     ; renew local melpa cache if fail
;;          (package-install 'leaf))))

;;     (leaf leaf
;;       :custom ((leaf-defaults . '(:ensure t))))

;;     (leaf leaf-keywords
;;       :ensure t
;;       :config (leaf-keywords-init)))

;;   (prog1 "optional packages for leaf-keywords"
;;     ;; optional packages if you want to use :hydra, :el-get,,,
;;     (leaf hydra :ensure t)
;;     (leaf el-get :ensure t
;;       :custom ((el-get-git-shallow-clone  . t)))))

;;; Benchmark init

(use-package benchmark-init
  :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'emacs-startup-hook 'benchmark-init/deactivate))

(add-to-list 'load-path elisp-directory)

;; (use-package use-package-git :demand t :ensure nil)

(use-package use-package-ensure-system-package :demand t)

(defun git-ls-files (&optional directory)
  "Return a list of the files from `git ls-files DIRECTORY'."
  (split-string (shell-command-to-string
                 (concat "git ls-files " (or directory default-directory)))))

(defun byte-compile-dotemacs ()
  "Byte compile all dotemacs Lisp files."
  (interactive)
  (let ((default-directory "~/.emacs.d/lisp"))
    (dolist (filename (git-ls-files))
      (byte-compile-file filename))))

(defun emacs-startup-message ()
  "Display a message after Emacs startup."
  (defconst emacs-load-time
    (float-time (time-subtract (current-time) emacs-start-time)))

  (message "Emacs loaded %d packages in %.1f seconds."
           (hash-table-count straight--recipe-cache)
           emacs-load-time))

(add-hook 'emacs-startup-hook #'emacs-startup-message)

(provide 'm-package)

;;; m-package.el ends here
