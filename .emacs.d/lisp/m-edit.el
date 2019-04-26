;;; m-edit.el --- Editing Functions -*- lexical-binding: t -*-

;; Author: Matthew Newton
;; Maintainer: Matthew Newton
;; Version: version
;; Package-Requires: (emacs "")
;; Homepage: https://gitlab.com/mnewt/dotemacs
;; Keywords: editing


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

;; commentary

;;; Code:

;; Use the system clipboard
(setq select-enable-clipboard t
      ;; Save existing system clipboard text into kill ring before replacing it,
      ;; ensuring it doesn't get irrevocably destroyed.
      save-interprogram-paste-before-kill t
      ;; use mouse to kill/yank
      mouse-yank-at-point t
      mouse-drag-and-drop-region t
      mouse-drag-and-drop-region-cut-when-buffers-differ t)

;; Wrap text.
(setq-default fill-column 80)

;; Newline at end of file.
(setq require-final-newline t
      ;; Sentences end with one space.
      sentence-end-double-space nil)

;; Delete selection on insert or yank
(delete-selection-mode 1)

;; Tabs
(setq-default indent-tabs-mode nil
              tab-width 2
              tab-stop-list (number-sequence tab-width 120 tab-width))

;; Automatically indent after RET
(electric-indent-mode +1)

(defun configure-auto-fill-mode ()
  "Automatically fill comments.
Wraps on `fill-column' columns.")

(add-hook 'prog-mode-hook (lambda ()
                            (set (make-local-variable 'comment-auto-fill-only-comments) t)
                            (auto-fill-mode t)))

;; http://whattheemacsd.com/key-bindings.el-03.html
;;;###autoload
(defun delete-indentation-forward ()
  "Like `delete-indentation', but in the opposite direction.
Bring the line below point up to the current line."
  (interactive)
  (join-line -1))

(use-package goto-chg
  :bind
  ("C-." . goto-last-change)
  ("C-;" . goto-last-change-reverse))

;; (use-package easy-kill
;;   :bind
;;   (([remap kill-ring-save] . easy-kill)
;;    ([remap mark-sexp] . easy-mark)))

(use-package ace-jump-mode
  :bind
  ("C-c SPC" . ace-jump-mode)
  ("C-c C-SPC" . ace-jump-mode-pop-mark))

(use-package ace-jump-zap
  :bind
  ("M-z" . ace-jump-zap-up-to-char-dwim)
  ("C-M-z" . ace-jump-zap-to-char-dwim))

(use-package mwim
  :bind
  ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
  ([remap move-end-of-line] . mwim-end-of-code-or-line))

(use-package expand-region
  :custom
  (expand-region-fast-keys-enabled nil)
  :bind
  ("s-d" . er/expand-region)
  ("C-=" . er/expand-region)
  ("s-D" . er/contract-region)
  ("C-+" . er/contract-region))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("M-s-m" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/unmark-next-like-this)
  ("C-M-<" . mc/mark-previous-like-this)
  ("C-M->" . mc/unmark-previous-like-this)
  ("C-c >" . mc/mark-all-dwim)
  ("C-c C-a"  . mc/mark-all-dwim))

(use-package move-text
  :bind
  (:map prog-mode-map
        ("M-S-<up>" . move-text-up)
        ("M-S-<down>" . move-text-down)))

(use-package string-inflection
  :bind
  ("C-c C-u" . string-inflection-all-cycle))

(use-package yasnippet
  :custom
  (yas-verbosity 1)
  :hook
  (after-init . yas-global-mode)
  (sh-mode . (lambda () (bind-key "C-c C-s" nil sh-mode-map)))
  :bind
  ("C-c C-s" . yas-insert-snippet))

(use-package yasnippet-snippets
  :defer 2)

(use-package flycheck
  :custom
  (flycheck-check-syntax-automatically '(save mode-enable))
  :hook
  (js2-mode . flycheck-mode)
  :bind
  ("C-c ! !" . flycheck-mode))

;; Some modes have their own formatting configuration.
(use-package format-all
  :hook
  ((css-mode
    dockerfile-mode
    emacs-lisp-mode
    enh-ruby-mode
    go-mode
    lua-mode
    php-mode
    ruby-mode
    sh-mode
    toml-mode
    web-mode
    yaml-mode) . format-all-mode))

(use-package parinfer
  :custom
  (parinfer-extensions
   '(defaults       ; should be included.
      pretty-parens ; different paren styles for different modes.
      smart-tab     ; C-b & C-f jump positions and smart shift with tab & S-tab.
      smart-yank))  ; Yank behavior depends on mode.
  :hook
  ((clojure-mode common-lisp-mode emacs-lisp-mode hy-mode lisp-interaction-mode
                 lisp-mode scheme-mode) . parinfer-mode)
  (parinfer-mode . (lambda () (parinfer-strategy-add 'default 'newline-and-indent)))
  :commands
  (parinfer-strategy-add)
  :bind
  (:map parinfer-mode-map
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)
        ("C-i" . parinfer--reindent-sexp)
        ("C-M-i" . parinfer-auto-fix)
        ("C-," . parinfer-toggle-mode)
        ;; Don't interfere with smartparens quote handling
        ("\"" . nil)
        ;; sp-newline seems to offer a better experience for lisps
        ("RET" . sp-newline)
        ("<return>" . sp-newline)
        :map parinfer-region-mode-map
        ("C-i" . indent-for-tab-command)
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)))

(defun sp-sh-post-handler (_id action _context)
  "Bash post handler ID, ACTION, CONTEXT."
  (-let (((&plist :arg arg :enc enc) sp-handler-context))
    (when (equal action 'barf-backward)
      (sp-ruby-delete-indentation 1)
      (indent-according-to-mode)
      (save-excursion
        (sp-backward-sexp)              ; move to begining of current sexp
        (sp-backward-sexp arg)
        (sp-ruby-maybe-one-space)))

    (when (equal action 'barf-forward)
      (sp-get enc
        (let ((beg-line (line-number-at-pos :beg-in))))))
    (sp-forward-sexp arg)
    (sp-ruby-maybe-one-space)
    (when (not (= (line-number-at-pos) beg-line))
      (sp-ruby-delete-indentation -1))
    (indent-according-to-mode)))

(defun sp-sh-block-post-handler (id action context)
  "Handler for bash block insertions.
ID, ACTION, CONTEXT."
  (when (equal action 'insert)
    (save-excursion
      (newline)
      (indent-according-to-mode))
    (indent-according-to-mode))
  (sp-sh-post-handler id action context))

(defun sp-sh-pre-handler (_id action _context)
  "Handler for bash slurp and barf.
ID, ACTION, CONTEXT."
  (let ((enc (plist-get sp-handler-context :enc)))
    (sp-get enc
      (let ((beg-line (line-number-at-pos :beg-in))))
      (end-line (line-number-at-pos :end-in)

                (when (equal action 'slurp-backward))))
    (save-excursion
      (sp-forward-sexp)
      (when (looking-at-p ";") (forward-char))
      (sp-ruby-maybe-one-space)
      (when (not (= (line-number-at-pos) end-line))
        (sp-ruby-delete-indentation -1)))
    (while (thing-at-point-looking-at "\\.[[:blank:]\n]*")
      (sp-backward-sexp))
    (when (looking-back "[@$:&?!]")
      (backward-char)
      (when (looking-back "[@&:]")
        (backward-char)))
    (just-one-space)
    (save-excursion
      (if (= (line-number-at-pos) end-line
             (insert " "))
          (newline

           (when (equal action 'barf-backward)))))
    ;; Barf whole method chains
    (while (thing-at-point-looking-at "[(.:[][\n[:blank:]]*")
      (sp-forward-sexp))
    (if (looking-at-p " *$")
        (newline)
      (save-excursion (newline)

                      (when (equal action 'slurp-forward))))
    (save-excursion
      (sp-backward-sexp)
      (when (looking-back "\." nil) (backward-char))
      (sp-ruby-maybe-one-space)
      (when (not (= (line-number-at-pos) beg-line))
        (if (thing-at-point-looking-at "\\.[[:blank:]\n]*")))
      (progn
        (forward-symbol -1)
        (sp-ruby-delete-indentation -1
                                    (sp-ruby-delete-indentation))))
    (while (looking-at-p "::") (sp-forward-symbol))
    (when (looking-at-p "[?!;]") (forward-char))
    (if (= (line-number-at-pos) beg-line)
        (insert " ")
      (newline

       (when (equal action 'barf-forward))))
    (when (looking-back "\\." nil) (backward-char))
    (while (looking-back "::" nil) (sp-backward-symbol))
    (if (= (line-number-at-pos) end-line)
        (insert " ")
      (if (looking-back "^[[:blank:]]*" nil
                        (save-excursion (newline)))
          (newline)))))

(defun sp-backward-slurp-into-previous-sexp ()
  "Add the sexp at point into the preceeding list."
  (interactive)
  (save-excursion
    (sp-down-sexp)
    (sp-backward-symbol)
    (sp-forward-slurp-sexp)))

;; See https://github.com/Fuco1/smartparens/issues/80
(defun sp-create-newline-and-enter-sexp (&rest _)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun toggle-sp-newline ()
  "Toggle whether `RET' is bound to `newline' or `sp-newline'.

This is because under certain conditions `sp-newline' can do bad
things."
  (interactive)
  (let* ((f (key-binding (kbd "RET")))
         (newf (if (eq f 'sp-newline) #'newline #'sp-newline)))
    (bind-key "RET" newf smartparens-mode-map)
    (message "<RET> now invokes to %s" newf)))

(use-package smartparens
  :custom
  (sp-hybrid-kill-entire-symbol nil)
  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (sp-cancel-autoskip-on-backward-movement nil)
  ;; smartparens does some weird stuff with bindings so you can't reliably use
  ;; `use-package/:bind' to set them.
  (sp-base-key-bindings 'paredit)
  (sp-override-key-bindings '(("C-M-<backspace>" . sp-splice-sexp-killing-backward)))
  :hook
  (smartparens-mode . (lambda ()
                        (require 'smartparens-config) (turn-on-show-smartparens-mode)))
  (prog-mode . turn-on-smartparens-mode)
  (clojure-mode . (lambda () (require 'smartparens-clojure)))
  ((ruby-mode enh-ruby-mode) . (lambda () (require 'smartparens-ruby)))
  ((javascript-mode js2-mode json-mode) . (lambda () (require 'smartparens-javascript)))
  (lua-mode . (lambda () (require 'smartparens-lua)))
  (markdown-mode . (lambda () (require 'smartparens-markdown)))
  (org-mode . (lambda () (require 'smartparens-org)))
  (python-mode . (lambda () (require 'smartparens-python)))
  (text-mode . (lambda () (require 'smartparens-text)))
  (web-mode . (lambda () (require 'smartparens-html)))
  (after-init
   .
   (lambda ()
     (sp-with-modes '(c-mode c++-mode css-mode graphql-mode javascript-mode
                             js2-mode json-mode objc-mode python-mode java-mode
                             sh-mode web-mode)
       (sp-local-pair "{" nil :post-handlers '((sp-create-newline-and-enter-sexp "RET")))
       (sp-local-pair "[" nil :post-handlers '((sp-create-newline-and-enter-sexp "RET")))
       (sp-local-pair "(" nil :post-handlers '((sp-create-newline-and-enter-sexp "RET"))))
     (sp-with-modes
         '(python-mode)
       (sp-local-pair "\"\"\"" "\"\"\""
                      :post-handlers '((sp-create-newline-and-enter-sexp "RET"))))
     (sp-with-modes
         '(sh-mode)
       (sp-local-pair "do" "done"
                      :when '(("SPC" "RET"))
                      :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
                      :actions '(insert navigate)
                      :pre-handlers '(sp-sh-pre-handler)
                      :post-handlers '(sp-sh-block-post-handler))
       (sp-local-pair "then" "fi"
                      :when '(("SPC" "RET"))
                      :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
                      :actions '(insert navigate)
                      :pre-handlers '(sp-sh-pre-handler)
                      :post-handlers '(sp-sh-block-post-handler))
       (sp-local-pair "case" "esac"
                      :when '(("SPC" "RET"))
                      :unless '(sp-in-string-p sp-in-comment-p sp-in-docstring-p)
                      :actions '(insert navigate)
                      :pre-handlers '(sp-sh-pre-handler)
                      :post-handlers '(sp-sh-block-post-handler))
       (smartparens-global-mode))))
  :commands
  (sp-local-pair sp-with-modes smartparens-global-mode turn-on-show-smartparens-mode)
  :bind
  (:map smartparens-mode-map
        ("C-c C-<return>" . toggle-sp-newline)
        :filter (cl-some #'derived-mode-p sp-lisp-modes)
        ([remap kill-line] . sp-kill-hybrid-sexp)))

(defun indent-buffer-or-region (beg end &optional arg)
  "Indent the region if one is active, otherwise format the buffer.
Some modes have special treatment."
  (interactive "rP")
  (when (sp-point-in-string-or-comment)
    (fill-paragraph arg))
  (call-interactively #'crux-cleanup-buffer-or-region)
  (if (use-region-p)
      (progn
        (indent-region beg end)
        (message "Region indented."))
    (progn
      (format-all-buffer)
      (message "Buffer formatted."))))

(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-buffer-or-region (region-beginning) (region-end)))
  (message "Defun indented."))

(defun clipboard-yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (if (and delete-selection-mode (use-region-p)) (delete-active-region))
  (clipboard-yank)
  (call-interactively 'indent-region))

(defun kill-line-or-region ()
  "Kill the current line or active region.

When `universal-argument' is called first, kill the whole
buffer (respects `narrow-to-region').

Stolen from `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun copy-line-or-region ()
  "Copy the current line or active region.

When called repeatedly, append copy subsequent lines. When
`universal-argument' is called first, copy whole
buffer (respects`narrow-to-region').

Stolen from `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'"
  (interactive)
  (if current-prefix-arg
      (progn
        (kill-ring-save (point-min) (point-max)))
    (if (use-region-p)
        (progn
          (kill-ring-save (region-beginning) (region-end)))
      (if (eq last-command this-command)
          (if (eobp)
              (progn)
            (progn
              (kill-append "\n" nil)
              (kill-append
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))
               nil)
              (progn
                (end-of-line)
                (forward-char))))
        (if (eobp)
            (if (eq (char-before) 10)
                (progn)
              (progn
                (kill-ring-save (line-beginning-position) (line-end-position))
                (end-of-line)))
          (progn
            (kill-ring-save (line-beginning-position) (line-end-position))
            (end-of-line)
            (forward-char)))))))

(defun comment-toggle ()
  "Toggle comments for the region.
If no region is selected, toggles comments for the line."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end))
  (if (bound-and-true-p parinfer-mode) (parinfer--invoke-parinfer)))

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (beginning-of-line)
  (set-mark (line-end-position)))

(defun move-line-or-region-to-other-window ()
  "Kill region (if active) or the current line then yank at point
in the other window."
  (interactive)
  (kill-line-or-region)
  (other-window 1)
  (yank)
  (newline)
  (other-window -1))

(defun copy-line-or-region-to-other-window ()
  "Copy region (if active) or the current line to point in the
other window."
  (interactive)
  (copy-line-or-region)
  (other-window 1)
  (yank)
  (newline)
  (other-window -1))

(bind-keys
 :map lisp-mode-shared-map
 ("s-<return>" . eval-last-sexp)
 ("C-s-<return>" . eval-last-sexp-other-window)
 ("C-c C-k" . eval-buffer)
 ("C-x C-r" . eval-region)
 ("C-x r E" . expression-to-register)
 ("C-x r e" . eval-register)
 ("C-M-\\" . indent-buffer-or-region)
 ("C-\\" . indent-defun)
 ("C-^" . delete-indentation-forward)
 ("s-C" . copy-line-or-region-to-other-window)
 ("s-X" . move-line-or-region-to-other-window)
 ;; Replace `delete-horizontal-space' with the more useful `cycle-spacing'.
 ("M-\\" . cycle-spacing)
 ;; Continue comment on next line (default binding is "C-M-j")
 ("M-RET" . indent-new-comment-line))

(provide 'm-edit)

;;; m-edit.el ends here
