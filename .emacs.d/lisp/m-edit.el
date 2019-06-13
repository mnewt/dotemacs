;;; m-edit.el --- Editing Functions -*- lexical-binding: t -*-

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
  :config
  (delete-selection-mode))

;; Automatically indent after RET
(use-package electric
  :defer 1
  :config
  (electric-indent-mode))

(defun auto-fill-mode-init ()
  "Automatically fill comments. Wraps on `fill-column' columns."
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'prog-mode-hook #'auto-fill-mode-init)

;; http://whattheemacsd.com/key-bindings.el-03.html
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
  :defer 5
  :custom
  ;; Don't write messages at startup.
  (yas-verbosity 1)
  :config
  (yas-global-mode)
  (with-eval-after-load 'sh-script
    (bind-keys :map sh-mode-map
               ("C-c C-s" . nil)
               ("C-c M-s" . sh-select)))
  :bind
  (:map yas-minor-mode-map
        ("s-'" . yas-expand)
        ("C-c C-s" . yas-insert-snippet)))

(use-package yasnippet-snippets
  :defer 5)

(use-package flyspell
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra"))
  :hook
  (text-mode . (lambda () (shut-up (flyspell-mode))))
  (prog-mode . (lambda () (shut-up (flyspell-prog-mode))))
  :bind
  (:map flyspell-mode-map
        ("C-," . nil)
        ("C-." . nil)
        ("C-;" . nil)
        ("C-M-i" . nil)))

(use-package flycheck
  :custom
  (flycheck-check-syntax-automatically '(idle-change idle-buffer-switch))
  (flycheck-idle-change-delay 1)
  (flycheck-idle-buffer-switch-delay 1)
  :config
  ;; Stolen from spacemacs
  (define-fringe-bitmap 'my-flycheck-fringe-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))

  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)

  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info)

  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
          :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
          :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("j"  flycheck-next-error                                       "Next")
    ("k"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil))
  
  :hook
  (prog-mode . flycheck-mode)
  :bind
  ("C-c ! !" . flycheck-mode))

;; Some modes have their own formatting configuration.
(use-package format-all
  :hook
  ((css-mode
    dockerfile-mode
    enh-ruby-mode
    go-mode
    ;; lua-mode ; Don't like the formatting.
    php-mode
    ruby-mode
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
  ((clojure-mode emacs-lisp-mode hy-mode lisp-interaction-mode
                 lisp-mode scheme-mode) . parinfer-mode)
  ;; (parinfer-mode . (lambda () (parinfer-strategy-add 'default 'newline-and-indent)))
  ;; :commands
  ;; (parinfer-strategy-add)
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
  :init
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
        (insert (format " in list; do\n%s\n" (s-repeat sh-basic-offset " ")))))
    (sp-sh-post-handler id action context))

  (defun sp-sh-if-post-handler (id action context)
    "Handler for bash if block insertions.
ID, ACTION, CONTEXT."
    (when (equal action 'insert)
      (save-excursion
        (insert (format " ; then\n%s\n" (s-repeat sh-basic-offset " "))))))

  (defun sp-sh-case-post-handler (id action context)
    "Handler for bash case block insertions.
ID, ACTION, CONTEXT."
    (when (equal action 'insert)
      (save-excursion
        (insert (format " in\n%s\*)\n%s\n%s;;\n"
                        (s-repeat sh-basic-offset " ")
                        (s-repeat (* 2 sh-basic-offset) " ")
                        (s-repeat (* 2 sh-basic-offset) " ")))))
    (sp-sh-post-handler id action context))

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

  ;; See https://github.com/Fuco1/smartparens/issues/80
  (defun sp-create-newline-and-enter-sexp (&rest _)
    "Open a new brace or bracket expression, with relevant newlines and indent."
    (message "sp-newline-etc")
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (defun smartparens-init ()
    "Initialize smartparens."
    (require 'smartparens-config)
    (sp-with-modes '(c-mode c++-mode css-mode graphql-mode javascript-mode js-mode
                            js2-mode json-mode objc-mode java-mode web-mode)
      (sp-local-pair "{" nil
                     :post-handlers '((sp-create-newline-and-enter-sexp "RET" sp-newline)))
      (sp-local-pair "[" nil
                     :post-handlers '((sp-create-newline-and-enter-sexp "RET" sp-newline)))
      (sp-local-pair "(" nil
                     :post-handlers '((sp-create-newline-and-enter-sexp "RET" sp-newline))))
    (sp-with-modes 'python-mode
      (sp-local-pair "\"\"\"" "\"\"\""
                     :post-handlers '((sp-create-newline-and-enter-sexp "RET" sp-newline))))
    (sp-with-modes 'sh-mode
      (sp-local-pair "{" nil
                     :post-handlers '((sp-create-newline-and-enter-sexp "RET" sp-newline)
                                      sp-sh-post-handler))
      (sp-local-pair "(" nil
                     :post-handlers '((sp-create-newline-and-enter-sexp "RET" sp-newline)
                                      sp-sh-post-handler))
      (sp-local-pair "[" "]"
                     :actions '(wrap insert navigate)
                     :post-handlers '(sp-sh-insert-spaces  sp-sh-post-handler))
      (sp-local-pair "[ " " ]"
                     :actions '(wrap insert navigate)
                     :post-handlers '(sp-sh-insert-spaces  sp-sh-post-handler))
      (sp-local-pair "[[" "]]"
                     :actions '(wrap insert navigate)
                     :post-handlers '(sp-sh-insert-spaces  sp-sh-post-handler))
      (sp-local-pair "[[ " " ]]" :actions '(wrap insert navigate)
                     :post-handlers '(sp-sh-insert-spaces  sp-sh-post-handler))
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
    (show-smartparens-global-mode))

  :custom
  ;; Don't kill the entire symbol with `sp-kill-hybrid-sexp'. If we want to kill
  ;; the entire symbol, use `sp-kill-symbol'.
  (sp-hybrid-kill-entire-symbol nil)
  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (sp-cancel-autoskip-on-backward-movement nil)
  ;; smartparens does some weird stuff with bindings so you can't reliably use
  ;; `use-package/:bind' to set them.
  (sp-base-key-bindings 'paredit)
  (sp-override-key-bindings '(("C-S-d" . sp-down-sexp)
                              ("C-M-d" . sp-kill-symbol)
                              ("M-<backspace>" . sp-backward-kill-word)
                              ("C-<backspace>" . sp-backward-kill-symbol)
                              ("C-M-<backspace>" . sp-backward-kill-sexp)
                              ("C-s-<backspace>" . sp-splice-sexp-killing-backward)))
  :config
  (smartparens-init)
  :commands
  (sp-local-pair sp-with-modes smartparens-global-mode show-smartparens-global-mode)
  :bind
  (:map prog-mode-map
        ("RET" . sp-newline)
        ("<return>" . sp-newline)
        :map lisp-mode-shared-map
        ([remap kill-line] . sp-kill-hybrid-sexp)
        (";" . sp-comment)))

(defun indent-buffer-or-region (beg end &optional arg)
  "Indent the region from BEG to END.

If no region is active, format the buffer.

Prefix ARG is passed to `fill-paragraph'."
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

(defface todo-face
  '((((class color))
     (:foreground "magenta" :weight bold))
    (t (:weight bold)))
  "Face to fontify FIXME/TODO words."
  :group 'todo)

(defun todo-add-font-locking ()
  "Add font locking for TODOs."
  (font-lock-add-keywords 'prog-mode '(("\\(TODO\\|FIXME\\|HELP\\):" 1 'todo-face t)) 'append))

(add-hook 'after-init-hook #'todo-add-font-locking)

(bind-keys
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
 ("M-RET" . indent-new-comment-line)
 ("M-z" . zap-to-char)
 ("M-Z" . zap-up-to-char))

(provide 'm-edit)

;;; m-edit.el ends here
