;;; m-package.el --- Package management -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


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

;; (use-package benchmark-init
;;   :demand t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'emacs-startup-hook 'benchmark-init/deactivate))

(add-to-list 'load-path elisp-directory)

(use-package use-package-git :demand t :ensure nil)

(use-package use-package-ensure-system-package :demand t)

;; TODO Need to make a common list of the files. git-ls?
(defun byte-compile-dotemacs ()
  "Byte compile all dotemacs Lisp files."
  (interactive)
  (dolist file ()))

(defun emacs-startup-message ()
  (defconst emacs-load-time
    (float-time (time-subtract (current-time) emacs-start-time)))

  (message "Emacs loaded %d packages in %.1f seconds."
           (+ (length package-activated-list) (length use-package-git--packages))
           emacs-load-time))


(add-hook 'emacs-startup-hook #'emacs-startup-message)

(provide 'm-package)

;;; m-package.el ends here
