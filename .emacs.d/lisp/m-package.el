;;; m-package.el --- Package management -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs Package Management

;;; Code:

;;;; use-package

(setq package-enable-at-startup nil
      package-user-dir "~/.emacs.d/packages/"
      package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/"))
      package-archive-priorities '(("org" . 30)
                                   ("melpa" . 20)
                                   ("elpa" . 10))
      custom-file "~/.emacs.d/custom.el")

(eval-when-compile
  (require 'package)
  (package-initialize)
  (custom-set-variables
   '(use-package-always-ensure t)
   '(use-package-always-defer t)
   '(use-package-enable-imenu-support t)
   '(use-package-hook-name-suffix nil))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

;; (use-package quelpa-use-package
;;   :demand t
;;   :init
;;   (defvar quelpa-use-package-inhibit-loading-quelpa t)
;;   (defvar quelpa-update-melpa-p nil)
;;   :config
;;   (quelpa-use-package-activate-advice))

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

(use-package use-package-git :demand t :ensure nil)

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
           (+ (length package-activated-list) (length use-package-git--packages))
           emacs-load-time))

(add-hook 'emacs-startup-hook #'emacs-startup-message)

(provide 'm-package)

;;; m-package.el ends here
