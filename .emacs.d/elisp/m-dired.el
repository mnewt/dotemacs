;;; m-dired.el --- My dired configuration -*- lexical-binding: t -*-

;; Author: Matthew Newton
;; Maintainer: Matthew Newton
;; Version: version
;; Package-Requires: ()
;; Homepage: homepage
;; Keywords: keywords


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

;; Loads dired customizations and related packages.

;;; Code:

(require 'dired)
(require 'dired-x)

;; try to use GNU ls on macOS since BSD ls doesn't explicitly support Emacs
(setq insert-directory-program (or (executable-find "gls")
                                   (executable-find "ls"))
      ;; don't prompt to kill buffers of deleted directories
      dired-clean-confirm-killing-deleted-buffers nil)

(use-package dired+)

(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-listing-switches "-alh"
      dired-dwim-target t
      dired-omit-mode t
      dired-omit-files "\\`[#.].*")

(setq-default dired-omit-files-p t)

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode t)))

(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))

(use-package diredfl
  :config
  (diredfl-global-mode))

(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("I" . dired-subtree-cycle)
        ("TAB" . dired-subtree-cycle)
        ("C-, i" . dired-subtree-insert)
        ("C-, r" . dired-subtree-remove)
        ("C-, R" . dired-subtree-revert)
        ("C-, n" . dired-subtree-narrow)
        ("C-, ^" . dired-subtree-up)
        ("C-, v" . dired-subtree-down)))

(use-package dired-rsync
  :bind
  (:map dired-mode-map
        ("C-c C-r" . dired-rsync)))

(use-package dired-sidebar
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  :bind
  (("C-x M-d" . dired-sidebar-toggle-sidebar)))

(use-package disk-usage
  :straight
  (:type git :host gitlab :repo "Ambrevar/emacs-disk-usage")
  :bind
  (:map dired-mode-map
        (")" . disk-usage-here)
        ("C-)" . disk-usage)))

(defun dired-open-file ()
  "Open file at point in OS default program."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (os-open-file file)))

(defvar-local dired-dotfiles-show-p t
  "If non-nil, show files beginning with `.' in dired.")

(defun dired-dotfiles-toggle ()
  "Toggle display of dot files."
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer)            ; otherwise just revert to re-show
             (setq-local dired-dotfiles-show-p t)))))

;;;###autoload
(defun dired-to-default-directory ()
  "Open directory containing the current file."
  (interactive)
  (dired default-directory))

(bind-keys
 ("C-x C-d" . dired-to-default-directory)
 ("C-x d" . dired)
 :map dired-mode-map
 ("C-c o" . dired-open-file)
 ("T" . touch)
 ("C-." . dired-omit-mode)
 ("C-c C-p" . wdired-change-to-wdired-mode))

(provide 'm-dired)

;;; m-dired.el ends here
