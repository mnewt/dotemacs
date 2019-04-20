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

(require 'dired-x)

(use-package dired+)

(use-package find-dired+)

(use-package ivy-dired-history
  :config
  (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)
  :bind
  (:map dired-mode-map
        ("," . dired)))

(setq dired-listing-switches "-alh"
      dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-dwim-target t
      dired-omit-mode t
      dired-omit-files "\\`[#.].*"
      ;; Try to use GNU ls on macOS since BSD ls doesn't explicitly support
      ;; Emacs and can run into issues with certain characters in the file name.
      insert-directory-program (or (executable-find "gls")
                                   (executable-find "ls"))
      ;; Don't prompt to kill buffers of deleted directories.
      dired-clean-confirm-killing-deleted-buffers nil
      find-ls-option '("-print0 | xargs -0 ls -alhd" . ""))

(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

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
  (("C-x d" . dired-sidebar-toggle-sidebar)))

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

(defun dired-make-available-human-readable ()
  (unless diredp-hide-details-last-state
    (save-excursion
      (goto-char (point-min))
      (let ((buffer-read-only nil)
            (avail (string-trim (replace-regexp-in-string
                                 "Avail" ""
                                 (shell-command-to-string "df --output=avail -h .")
                                 t t))))
        (when (re-search-forward
               "^\\s-*total used in directory [0-9BGKM]+ available \\([0-9]+\\)"
               nil t)
          (replace-match avail t nil nil 1))))))

(add-hook 'dired-after-readin-hook #'dired-make-available-human-readable)

(bind-keys
 :map dired-mode-map
 ("C-c o" . dired-open-file)
 ("T" . touch)
 ("C-." . dired-omit-mode)
 ("C-c C-p" . wdired-change-to-wdired-mode))

(provide 'm-dired)

;;; m-dired.el ends here
