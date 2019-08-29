;;; edit.el --- Editing Functions -*- lexical-binding: t -*-

;;; Commentary:

;; General editing related configuration and functionality

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

;; Tabs
(setq-default indent-tabs-mode nil
              tab-width 2
              tab-stop-list (number-sequence tab-width 120 tab-width))

;; Delete selection on insert or yank
(use-package delsel
  :defer 1
  :commands
  delete-selection-mode delete-active-region
  :config
  (delete-selection-mode))

;; Automatically indent after RET
(use-package electric
  :defer 1
  :commands
  electric-indent-mode
  :config
  (electric-indent-mode))

(defun auto-fill-mode-init ()
  "Automatically fill comments. Wraps on `fill-column' columns."
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'prog-mode-hook #'auto-fill-mode-init)

(use-package so-long
  :defer 1
  :if (>= emacs-major-version 27)
  :ensure nil
  :config
  (global-so-long-mode))

(use-package unfill
  :commands
  (unfill-region unfill-paragraph)
  :bind
  (:map prog-mode-map
        ("M-q" . unfill-toggle)))

;; http://whattheemacsd.com/key-bindings.el-03.html
(defun delete-indentation-forward ()
  "Like `delete-indentation', but in the opposite direction.
Bring the line below point up to the current line."
  (interactive)
  (join-line -1))

(use-package undo-redo
  :git "https://github.com/clemera-dev/undo-redo"
  :bind
  ("s-z" . undo-modern)
  ("s-Z" . redo))

(use-package undohist
  :git "https://github.com/clemera-dev/undohist"
  :defer 1
  :config
  (undohist-initialize))

(use-package undo-propose
  :bind
  ("M-s-z" . undo-propose))

(use-package volatile-highlights
  :defer 2
  :config
  (vhl/define-extension 'undo-redo 'undo-modern 'undo)
  (vhl/install-extension 'undo-redo)
  (volatile-highlights-mode t))

(use-package goto-chg
  :defer 1
  :bind
  ("C-." . goto-last-change)
  ("C-;" . goto-last-change-reverse))

(use-package easy-kill
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark)))

(use-package mwim
  :bind
  ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
  ([remap move-end-of-line] . mwim-end-of-code-or-line))

(use-package expand-region
  :after org
  :custom
  (expand-region-fast-keys-enabled nil)
  :bind
  ("s-d" . er/expand-region)
  ("C-=" . er/expand-region)
  ("s-D" . er/contract-region)
  ("C-+" . er/contract-region))

(use-package multiple-cursors
  :commands
  mc/add-cursor-on-click
  :config
  (defhydra hydra-multiple-cursors (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
_p_   Next    _n_   Next    _l_ Edit lines
_P_   Skip    _N_   Skip    _a_ Mark all
_M-p_ Unmark  _M-n_ Unmark  _r_ Mark by regexp
^ ^             ^ ^             _q_ Quit
"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil)
    ("<mouse-1>" mc/add-cursor-on-click)
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore))

  :bind
  ("M-<down-mouse-1>" . mc/add-cursor-on-click)
  ("C-S-c C-S-c" . mc/edit-lines)
  ("M-s-m" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/unmark-next-like-this)
  ("C-M-<" . mc/mark-previous-like-this)
  ("C-M->" . mc/unmark-previous-like-this)
  ("C-c >" . mc/mark-all-dwim)
  ("C-c C-a"  . mc/mark-all-dwim)
  ("C-'" . mc-hide-unmatched-lines-mode))

(use-package replace
  :ensure nil
  :commands
  occur-next
  occur-prev
  :config
  (defun occur-dwim ()
    "Call `occur' with a sane default, chosen as the thing under point or selected region."
    (interactive)
    (push (if (region-active-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))
            (let ((sym (thing-at-point 'symbol)))
              (when (stringp sym)
                (regexp-quote sym))))
          regexp-history)
    (call-interactively 'occur))

  (declare-function other-window-hydra-occur 'hydra)

  (advice-add 'occur-mode-goto-occurrence :after #'other-window-hydra-occur)

  ;; Focus on *Occur* window right away.
  (add-hook 'occur-hook (lambda () (other-window 1)))

  (defun reattach-occur ()
    "Switch to Occur buffer and launch the hydra."
    (if (get-buffer "*Occur*")
        (switch-to-buffer-other-window "*Occur*")
      (hydra-occur-dwim/body)))

  ;; Used in conjunction with occur-mode-goto-occurrence-advice this helps keep
  ;; focus on the *Occur* window and hides upon request in case needed later.
  (defhydra hydra-occur-dwim ()
    "Occur mode"
    ("o" occur-dwim "Start occur-dwim" :color red)
    ("j" occur-next "Next" :color red)
    ("k" occur-prev "Prev":color red)
    ("h" delete-window "Hide" :color blue)
    ("r" (reattach-occur) "Re-attach" :color red)
    ("q" nil))

  :bind
  (:map occur-mode-map
   ("C-o" . hydra-occur-dwim/body)))

(use-package move-text
  :bind
  (:map prog-mode-map
   ("M-S-<up>" . move-text-up)
   ("M-S-<down>" . move-text-down)))

(use-package string-inflection
  :bind
  ("C-c C-u" . string-inflection-all-cycle))

(use-package yasnippet
  :defer 5
  :custom
  ;; Don't write messages at startup.
  (yas-verbosity 1)
  :commands
  yas-global-mode
  :config
  (yas-global-mode)
  :bind
  (:map yas-minor-mode-map
        ("s-'" . yas-expand)
        ("C-c C-s" . yas-insert-snippet)))

(use-package yasnippet-snippets
  :defer 5)

(use-package parinfer
  :custom
  (parinfer-extensions
   '(defaults       ; should be included.
      pretty-parens ; different paren styles for different modes.
      smart-tab     ; C-b & C-f jump positions and smart shift with tab & S-tab.
      smart-yank))  ; Yank behavior depends on mode.
  :config
  (parinfer-strategy-add 'default 'newline-and-indent)
  :hook
  ((clojure-mode-hook
    emacs-lisp-mode-hook
    hy-mode-hook
    lisp-interaction-mode-hook
    lisp-mode-hook
    scheme-mode-hook) . parinfer-mode)
  :commands
  (parinfer-strategy-add parinfer--invoke-parinfer)
  :bind
  (:map parinfer-mode-map
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)
        ("C-," . parinfer-toggle-mode)
        ;; Don't interfere with smartparens quote handling
        ("\"" . nil)
        ;; sp-newline seems to offer a better experience for lisps
        ("RET" . nil)
        ("<return>" . nil)
        :map parinfer-region-mode-map
        ("C-i" . indent-for-tab-command)
        ("<tab>" . parinfer-smart-tab:dwim-right)
        ("S-<tab>" . parinfer-smart-tab:dwim-left)))

(use-package smartparens
  :defer 1
  :custom
  ;; Don't kill the entire symbol with `sp-kill-hybrid-sexp'. If we want to kill
  ;; the entire symbol, use `sp-kill-symbol'.
  (sp-hybrid-kill-entire-symbol nil)
  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (sp-cancel-autoskip-on-backward-movement nil)
  :commands
  sp-get-pair
  sp--get-opening-regexp
  sp--get-closing-regexp
  :config
  (eval-when-compile (require 'sh-script))

  (defun sp-add-space-after-sexp-insertion (id action _context)
    "Add space after sexp insertion.
ID, ACTION, CONTEXT."
    (when (eq action 'insert)
      (save-excursion
        (forward-char (sp-get-pair id :cl-l))
        (when (or (eq (char-syntax (following-char)) ?w)
                  (looking-at (sp--get-opening-regexp)))
          (insert " ")))))

  (defun sp-add-space-before-sexp-insertion (id action _context)
    "Add space before sexp insertion.
ID, ACTION, CONTEXT."
    (when (eq action 'insert)
      (save-excursion
        (backward-char (length id))
        (when (or (eq (char-syntax (preceding-char)) ?w)
                  (and (looking-back (sp--get-closing-regexp) nil)
                       (not (eq (char-syntax (preceding-char)) ?'))))
          (insert " ")))))

  (defun sp-sh-post-handler (_id action _context)
    "Bash post handler.
ID, ACTION, CONTEXT."
    (-let (((&plist :arg arg :enc enc) sp-handler-context))
      (when (equal action 'barf-backward)
        (delete-indentation 1)
        (indent-according-to-mode)
        (save-excursion
          (sp-backward-sexp)
          (sp-backward-sexp arg)
          (just-one-space)))
      (when (equal action 'barf-forward)
        (sp-get enc
          (let ((beg-line (line-number-at-pos :beg-in)))
            ;; (end-line (line-number-at-pos :end-in)))
            (sp-forward-sexp arg)
            (just-one-space)
            (when (not (= (line-number-at-pos) beg-line))
              (delete-indentation -1))
            (indent-according-to-mode))))
      (when (eq action 'insert)
        (save-excursion
          (indent-according-to-mode)))))

  (defun sp-sh-for-post-handler (id action context)
    "Handler for bash for block insertions.
ID, ACTION, CONTEXT."
    (when (equal action 'insert)
      (save-excursion
        (insert (format " in list; do\n%s\n" (make-string sh-basic-offset ?\s)))))
    (sp-sh-post-handler id action context))

  (defun sp-sh-if-post-handler (_id action _context)
    "Handler for bash if block insertions.
ID, ACTION, CONTEXT."
    (when (equal action 'insert)
      (save-excursion
        (insert (format " ; then\n%s\n" (make-string sh-basic-offset ?\s))))))

  (defun sp-sh-case-post-handler (id action context)
    "Handler for bash case block insertions.
ID, ACTION, CONTEXT."
    (when (equal action 'insert)
      (save-excursion
        (insert (format " in\n%s\*)\n%s\n%s;;\n"
                        (make-string sh-basic-offset ?\s)
                        (make-string (* 2 sh-basic-offset) ?\s)
                        (make-string (* 2 sh-basic-offset) ?\s)))))
    (sp-sh-post-handler id action context))

  (declare-function thing-at-point-looking-at 'thingatpt)

  (defun sp-sh-pre-handler (_id action _context)
    "Handler for sh slurp and barf.
ID, ACTION, CONTEXT."
    (let ((enc (plist-get sp-handler-context :enc)))
      (sp-get enc
        (let ((beg-line (line-number-at-pos :beg-in))
              (end-line (line-number-at-pos :end-in)))
          (when (equal action 'slurp-backward)
            (save-excursion
              (sp-forward-sexp)
              (when (looking-at-p ";") (forward-char))
              (just-one-space)
              (when (not (= (line-number-at-pos) end-line))
                (delete-indentation -1))))
          (when (equal action 'barf-backward)
            ;; Barf whole method chains
            (while (thing-at-point-looking-at "[(.:[][\n[:blank:]]*")
              (sp-forward-sexp))
            (if (looking-at-p " *$")
                (newline)
              (save-excursion (newline))))
          (when (equal action 'slurp-forward)
            (save-excursion
              (sp-backward-sexp)
              (just-one-space)
              (when (not (= (line-number-at-pos) beg-line))
                (delete-indentation)))
            (when (looking-at-p ";") (forward-char))
            (if (= (line-number-at-pos) beg-line)
                (insert " ")
              (newline)))
          (when (equal action 'barf-forward)
            (if (= (line-number-at-pos) end-line)
                (insert " ")
              (if (looking-back "^[[:blank:]]*" nil)
                  (save-excursion (newline))
                (newline))))))))

  (defun sp-sh-insert-spaces (_id action _context)
    "Handler for sh wrap.
ID, ACTION, CONTEXT."
    (when (eq action 'wrap)
      (save-excursion
        (goto-char (sp-get sp-last-wrapped-region :end-in))
        (unless (looking-at "]]")
          (insert " "))
        (goto-char (sp-get sp-last-wrapped-region :beg-in))
        (unless (looking-back "\\[\\[" nil)
          (insert " ")))))

  (defun sp-backward-slurp-into-previous-sexp ()
    "Add the sexp at point into the preceeding list."
    (interactive)
    (save-excursion
      (sp-down-sexp)
      (sp-backward-symbol)
      (sp-forward-slurp-sexp)))

  (defun sp-create-newline-and-enter-sexp (&rest _)
    "Open a new brace or bracket expression, with relevant newlines and indent.

See https://github.com/Fuco1/smartparens/issues/80."
    (message "sp-newline-etc")
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (require 'smartparens-config)

  ;; Smartparens is broken in `cc-mode' as of Emacs 27. See
  ;; https://github.com/Fuco1/smartparens/issues/963
  (when (version<= "27" emacs-version)
    (dolist (fun '(c-electric-paren c-electric-brace))
      (add-to-list 'sp--special-self-insert-commands fun)))

  (sp-with-modes '(c-mode c++-mode csharp-mode css-mode graphql-mode
                          javascript-mode js-mode js2-mode json-mode
                          objc-mode java-mode web-mode)
    (sp-local-pair "{" nil
                   :post-handlers
                   '((sp-create-newline-and-enter-sexp "RET" newline-and-indent)))
    (sp-local-pair "[" nil
                   :post-handlers
                   '((sp-create-newline-and-enter-sexp "RET" newline-and-indent)))
    (sp-local-pair "(" nil
                   :post-handlers
                   '((sp-create-newline-and-enter-sexp "RET" newline-and-indent))))
  (sp-with-modes 'python-mode
    (sp-local-pair "\"\"\"" "\"\"\""
                   :post-handlers
                   '((sp-create-newline-and-enter-sexp "RET" newline-and-indent))))
  (sp-with-modes 'sh-mode
    (sp-local-pair "{" nil
                   :post-handlers
                   '((sp-create-newline-and-enter-sexp "RET" newline-and-indent)
                     sp-sh-post-handler))
    (sp-local-pair "(" nil
                   :post-handlers
                   '((sp-create-newline-and-enter-sexp "RET" newline-and-indent)
                     sp-sh-post-handler))
    (sp-local-pair "[" "]"
                   :actions '(wrap insert navigate)
                   :post-handlers '(sp-sh-insert-spaces sp-sh-post-handler))
    (sp-local-pair "[ " " ]"
                   :actions '(wrap insert navigate)
                   :post-handlers '(sp-sh-insert-spaces sp-sh-post-handler))
    (sp-local-pair "[[" "]]"
                   :actions '(wrap insert navigate)
                   :post-handlers '(sp-sh-insert-spaces sp-sh-post-handler))
    (sp-local-pair "[[ " " ]]" :actions '(wrap insert navigate)
                   :post-handlers '(sp-sh-insert-spaces sp-sh-post-handler))
    (sp-local-pair "for" "done"
                   :when '(("SPC" "RET" "TAB" sp-newline))
                   :unless '(sp-in-string-p sp-in-comment-p)
                   :actions '(insert navigate)
                   :pre-handlers '(sp-sh-pre-handler)
                   :post-handlers '(sp-sh-for-post-handler))
    (sp-local-pair "if" "fi"
                   :when '(("SPC" "RET" "TAB" sp-newline))
                   :unless '(sp-in-string-p sp-in-comment-p)
                   :actions '(insert navigate)
                   :pre-handlers '(sp-sh-pre-handler)
                   :post-handlers '(sp-sh-if-post-handler))
    (sp-local-pair "case" "esac"
                   :when '(("SPC" "RET" "TAB" sp-newline))
                   :unless '(sp-in-string-p sp-in-comment-p)
                   :actions '(insert navigate)
                   :pre-handlers '(sp-sh-pre-handler)
                   :post-handlers '(sp-sh-case-post-handler)))
  (sp-with-modes 'org-mode
    (sp-local-pair "=" "=" :wrap "C-M-=")
    (sp-local-pair "~" "~" :wrap "C-~"))
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-with-modes sp-lisp-modes
    (sp-local-pair "(" nil
                   :wrap "C-M-("
                   :pre-handlers '(sp-add-space-before-sexp-insertion)
                   :post-handlers '(sp-add-space-after-sexp-insertion)))
  (setq sp-ignore-modes-list
        (delete 'minibuffer-inactive-mode sp-ignore-modes-list))
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  :commands
  sp-local-pair
  sp-with-modes
  smartparens-global-mode
  show-smartparens-global-mode
  sp-point-in-string-or-comment sp-forward-slurp-sexp
  sp-backward-symbol sp-backward-symbol sp-down-sexp
  sp-forward-sexp sp-backward-sexp
  :bind
  (:map lisp-mode-shared-map
        ("RET" . sp-newline)
        ("<return>" . sp-newline)
        ("C-k" . sp-kill-hybrid-sexp)
        (";" . sp-comment))
  (:map smartparens-mode-map
        ("M-s" . nil)
        ("M-r" . nil)
        ("M-<up>" . nil)
        ("M-<down>" . nil)
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-n" . sp-next-sexp)
        ("C-M-p" . sp-previous-sexp)
        ("M-a" . sp-beginning-of-sexp)
        ("M-e" . sp-end-of-sexp)
        ("C-M-d" . sp-down-sexp)
        ("C-M-u" . sp-backward-up-sexp)
        ("M-s-s" . sp-splice-sexp)
        ("C-S-d" . sp-kill-symbol)
        ("C-M-k" . sp-kill-sexp)
        ("C-M-w" . sp-copy-sexp)
        ("C-M-t" . sp-transpose-sexp)
        ("C-M-SPC" . sp-mark-sexp)
        ("M-<backspace>" . sp-backward-kill-word)
        ("C-<backspace>" . sp-backward-kill-symbol)
        ("C-M-<backspace>" . sp-backward-kill-sexp)
        ("C-s-<backspace>" . sp-splice-sexp-killing-backward)
        ("M-(" . sp-wrap-round)
        ("M-[" . sp-wrap-square)
        ("M-{" . sp-wrap-qurly)
        ("M-<delete>" . sp-unwrap-sexp)
        ("M-<backspace>" . sp-backward-delete-word)
        ("C-)" . sp-forward-slurp-sexp)
        ("C-}" . sp-forward-barf-sexp)
        ("C-(" . sp-backward-slurp-sexp)
        ("C-{" . sp-backward-barf-sexp)
        ("M-k" . sp-split-sexp)
        ("M-j" . sp-join-sexp)
        ("C-c s a" . sp-absorb-sexp)
        ("C-c s e" . sp-emit-sexp)
        ("C-c s p" . sp-convolute-sexp)
        ("C-c s t" . sp-transpose-hybrid-sexp)
        ("C-c s (" . sp-rewrap-sexp)
        ("C-c s r" . sp-change-inner)
        ("C-c s s" . sp-change-enclosing)))

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
  "Kill region or line then yank at point in the other window."
  (interactive)
  (kill-line-or-region)
  (other-window 1)
  (yank)
  (newline)
  (other-window -1))

(defun copy-line-or-region-to-other-window ()
  "Copy region line to point in the other window."
  (interactive)
  (copy-line-or-region)
  (other-window 1)
  (yank)
  (newline)
  (other-window -1))

(bind-keys
 ("C-M-}" . forward-sentence)
 ("C-M-{" . backward-sentence)
 ("C-^" . delete-indentation-forward)
 ("s-C" . copy-line-or-region-to-other-window)
 ("s-X" . move-line-or-region-to-other-window)
 ;; Replace `delete-horizontal-space' with the more useful `cycle-spacing'.
 ("M-\\" . cycle-spacing)
 ;; Continue comment on next line (default binding is "C-M-j")
 ("M-RET" . indent-new-comment-line)
 ("M-z" . zap-to-char)
 ("M-Z" . zap-up-to-char))

(provide 'm-edit)

;;; edit.el ends here
