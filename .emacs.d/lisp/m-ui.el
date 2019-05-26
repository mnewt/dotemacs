;;; m-ui.el --- User Interface -*- lexical-binding: t -*-

;;; Commentary:

;; Handle how the user interacts with Emacs

;;; Code:

;; Full screen
(defun fullscreen ()
  "Toggle fullscreen mode."
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(bind-keys ("C-s-f" . fullscreen))

;; Change yes/no prompts to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable all commands
(setq disabled-command-function nil)

(defun undo-tree-keep-region (f &rest args)
  "Keep region after `undo-tree-undo'.
Call F with ARGS."
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        (apply f args)
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    (call-interactively f)))

(use-package undo-tree
  :straight
  (:type git :host nil :repo "http://www.dr-qubit.org/git/undo-tree.git")
  :init
  ;; Keep region when undoing in region.
  ;; http://whattheemacsd.com/my-misc.el-02.html
  (advice-add 'undo-tree-undo :around #'undo-tree-keep-region)
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  :hook
  (after-init . global-undo-tree-mode)
  :bind
  (("s-z" . undo-tree-undo)
   ("s-Z" . undo-tree-redo)
   ("s-y" . undo-tree-redo)
   ("M-s-z" . undo-tree-visualize)))

(defun evil-mode-toggle ()
  "Toggle `evil-mode'.
It's necessary because it often forgets to change the cursor type back."
  (interactive)
  (if (bound-and-true-p evil-state)
      (progn
        (call-interactively #'turn-off-evil-mode)
        (setq cursor-type 'box))
    (call-interactively #'turn-on-evil-mode)))

(use-package evil
  :commands
  (turn-on-evil-mode turn-off-evil-mode)
  :bind
  ("s-ESC" . evil-mode-toggle)
  ("s-<escape>" . evil-mode-toggle))

(provide 'm-ui)

;;; m-ui.el ends here
