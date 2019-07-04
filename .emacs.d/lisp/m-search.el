;;; m-search.el --- Search Features -*- lexical-binding: t -*-

;;; Commentary:

;; Search and Project Management functionality

;;; Code:

(defun replace-regexp-entire-buffer (pattern replacement)
  "Immediately replace PATTERN with REPLACEMENT throughout the buffer."
  (interactive
   (let ((args (query-replace-read-args "Replace in entire buffer" t)))
     (setcdr (cdr args) nil)    ; remove third value returned from query---args
     args))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match replacement))))

;; Show line in the original buffer from occur mode
(setq list-matching-lines-jump-to-current-line t)

(use-package re-builder
  :defer 5
  :custom
  ;; string syntax means you don't need to double escape things.
  (reb-re-syntax 'string)
  :config
  (use-package pcre2el
    :hook
    ((emacs-lisp-mode-hook lisp-interaction-mode-hook reb-mode-hook) . rxt-mode))
  :bind
  ("C-c r" . re-builder))

(use-package wgrep
  :defer 5
  :custom
  ;; Save changed buffers immediately when exiting wgrep mode
  (wgrep-auto-save-buffer t)
  :bind
  (:map grep-mode-map
        ("C-c C-p" . wgrep-change-to-wgrep-mode)
        :map occur-mode-map
        ("C-c C-p" . wgrep-change-to-wgrep-mode)))

(use-package rg
  :defer 2
  :after
  wgrep-ag
  :ensure-system-package
  (rg . ripgrep)
  :custom
  (rg-keymap-prefix (kbd "C-c M-s"))
  :commands
  rg-enable-default-bindings
  :config
  (rg-enable-default-bindings (kbd "C-r"))
  :hook
  (rg-mode-hook . wgrep-ag-setup))

(use-package ivy
  :defer 0.5
  :custom
  (enable-recursive-minibuffers t)
  (ivy-display-style 'fancy)
  (ivy-count-format "[%d/%d] ")
  ;; Don't exit the minibuffer and pressing backspace on an empty line.
  (ivy-on-del-error-function (lambda (&rest _) nil))
  :commands
  ivy-mode
  ivy-set-index
  ivy-set-actions
  ivy-add-actions
  ivy--reset-state
  :config
  (defun ivy-quit-or-delete-char (arg)
    "Quit Ivy if `C-d' is pressed on empty line, otherwise pass ARG on."
    (interactive "p")
    (if (and (eolp) (looking-back (replace-regexp-in-string "\\[[^]]*\\]" "" ivy--prompt) nil))
        (progn
          (abort-recursive-edit))
      (delete-char arg)))

  (ivy-mode)
  :bind
  (:map ivy-mode-map
        ("C-c C-r" . ivy-resume)
        :map ivy-minibuffer-map
        ("C-e" . ivy-partial-or-done)
        ("C-d" . ivy-quit-or-delete-char)))

(use-package ivy-hydra
  :defer 1
  :after (ivy hydra))

(use-package swiper
  :defer 0.5
  :after ivy
  :bind
  (:map ivy-minibuffer-map
        ("C-c C-c" . ivy-toggle-calling)
        ("s-5" . ivy--replace-regexp-entire-buffer)))

(use-package counsel
  :defer 0.5
  :after ivy
  :custom
  (counsel-find-file-at-point t)
  (counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :commands
  counsel-mode
  :config
  
  (defun counsel-find-file-edit-path ()
    "Make the path in `counsel-find-file' editable."
    (interactive)
    (let ((dir ivy--directory))
      (setq ivy--old-cands nil)
      (setq ivy--old-re nil)
      (ivy-set-index 0)
      (setq ivy--directory "")
      (setq ivy--all-candidates nil)
      (setq ivy-text "")
      (delete-minibuffer-contents)
      (insert dir)))

  (defun ivy--call-with-current-buffer-in-other-window-action (x)
    "Switch to other window and call command X."
    (switch-to-buffer-other-window (current-buffer)
                                   (call-interactively (intern x))))

  (defun ivy--call-with-other-window-action (x)
    "Switch current buffer to other window and call command X."
    (other-window 1)
    (call-interactively (intern x)))

  (defun counsel-rg-default-directory (f &rest args)
    "Call F (`counsel-rg') with ARGS from `default-directory'.

NOTE: It seems like `counsel-rg' should call itself from
`default-directory' without assistance but in my experience it
looks for a project root directory instead. If we want to search
from the project root, we can use `counsel-projectile-rg', and we
force `counsel-rg' to search in `default-directory.'"
    (let ((initial-input (car args))
          (initial-directory (or (cadr args) default-directory))
          (extra-rg-args (caddr args))
          (rg-prompt (or (cadddr args) (format "(%s) rg: " default-directory))))
      (funcall f initial-input initial-directory extra-rg-args rg-prompt)))
  
  (advice-add #'counsel-rg :around #'counsel-rg-default-directory)

  (defun reloading (cmd)
    "Wrap CMD, reloading ivy."
    (lambda (x)
      (funcall cmd x)
      (ivy--reset-state ivy-last)))

  (defun given-file (cmd prompt)
    "Wrap in a closure and call CMD, with interactive PROMPT."
    (lambda (source)
      (let ((target
             (let ((enable-recursive-minibuffers t))
               (read-file-name
                (format "%s %s to:" prompt source)))))
        (funcall cmd source target 1))))

  (defun confirm-delete-file (file)
    "Delete FILE with confirmation."
    (dired-delete-file file 'confirm-each-subdirectory))

  (ivy-add-actions
   'counsel-M-x
   `(("j" ivy--call-with-current-buffer-in-other-window-action "other window")))
  (ivy-add-actions
   'counsel-find-file
   `(("c" ,(given-file #'copy-file "Copy") "copy")
     ("m" ,(reloading (given-file #'rename-file "Move")) "move")))
  (ivy-add-actions
   'counsel-projectile-find-file
   `(("c" ,(given-file #'copy-file "Copy") "copy")
     ("m" ,(reloading (given-file #'rename-file "Move")) "move")
     ("b" counsel-find-file-cd-bookmark-action "cd bookmark")))
  (ivy-add-actions
   'counsel-switch-buffer
   '(("f" ivy--find-file-action "find file")
     ("j" ivy--switch-buffer-other-window-action "other window")
     ("k" ivy--kill-buffer-action "kill")
     ("r" ivy--rename-buffer-action "rename")))
  ;; TODO: Adapt these functions for `counsel-rg'. See `counsel-git-grep-action'.
  ;; (ivy-add-actions
  ;;  'counsel-rg
  ;;  '(("f" ivy--find-file-action "find file")
  ;;    ("j" ivy--call-with-other-window-action "other window")))
  (ivy-set-actions
   'counsel-register
   '(("d" counsel-register-action-delete "delete")
     ("k" counsel-register-action-then-delete "call then delete")))

  (counsel-mode)
  :bind
  (:map counsel-mode-map
        ("M-x" . counsel-M-x)
        ("C-h C-k" . counsel-descbinds)
        ("s-f" . counsel-grep-or-swiper)
        ("s-F" . counsel-rg)
        ("C-x C-f" . counsel-find-file)
        ("C-x f" . counsel-recentf)
        ("C-x j" . counsel-file-jump)
        ("s-B" . counsel-switch-buffer)
        ("C-s-b" . counsel-switch-buffer-other-window)
        ("C-h <tab>" . counsel-info-lookup-symbol)
        ("C-h C-a" . counsel-apropos)
        ("C-c u" . counsel-unicode-char)
        ("C-c g" . counsel-git)
        ("C-c j" . counsel-git-grep)
        ("C-c M-o" . counsel-outline)
        ("M-s-v" . counsel-yank-pop)
        ("M-Y" . counsel-yank-pop)
        ("s-r" . counsel-imenu)
        ;; Don't shadow default binding.
        ([remap yank-pop] . nil)
        ("M-Y" . counsel-yank-pop)
        ([remap find-file] . counsel-find-file))
  (:map ivy-minibuffer-map
        ("M-<backspace>" . counsel-up-directory)
        ("M-DEL" . counsel-up-directory)
        ("C-c C-f" . counsel-find-file-edit-path))
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuffer-history)))

(use-package projectile
  :defer 1
  :custom
  (projectile-keymap-prefix (kbd "C-c p"))
  (projectile-completion-system 'ivy)
  (projectile-project-search-path (list code-directory))
  (projectile-globally-ignored-files '("TAGS" "package-lock.json"))
  (projectile-switch-project-action 'projectile-dired)
  :commands
  projectile-mode
  projectile-project-root
  :config
  (defun projectile-load-settings (&optional file)
    "Load project elisp settings from FILE.
Look in active project root directory, or if in the case of
  undefined root directory, file is otherwise path resolvable.

https://github.com/jfeltz/projectile-load-settings/blob/master/projectile-load-settings.el"
    (interactive)
    (let ((p (expand-file-name (or file "config.el")
                               (and (fboundp #'projectile-project-root)
                                    (projectile-project-root)))))
      (when (file-exists-p p)
        (load p)
        (message "%s" (concat "Loaded project settings from: " p)))))

  (defun projectile-git-ls-files (&optional dir)
    "List of the tracked files in the git repo, specified by DIR."
    (cd (or dir (projectile-project-root)))
    (cl-remove-if #'string-blank-p
                  (split-string (shell-command-to-string "git ls-files") "\n")))

  (defun projectile-git-ls-files-dired (&optional dir)
    "Dired list of the tracked files in the git repo, specified by DIR."
    (interactive)
    (let ((dir (or dir (projectile-project-root))))
      (dired (cons dir (projectile-git-ls-files dir)))
      (rename-buffer (format "*git ls-files %s*" dir))))
  
  ;; Why doesn't projectile have this as a default?
  (projectile-register-project-type 'generic nil
                                    :compile ""
                                    :test ""
                                    :test-suffix "_test")
  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm start"
                                    :test "npm test"
                                    :test-suffix ".test")
  (projectile-register-project-type 'clojure-cli '("deps.edn")
                                    :compile "clj "
                                    :test-suffix "_test")
  (projectile-mode)
  :hook
  (projectile-after-switch-project-hook . projectile-load-settings)
  :commands
  projectile-register-project-type
  :bind
  (:map projectile-mode-map
        ("s-}" . projectile-next-project-buffer)
        ("C-c }" . projectile-next-project-buffer)
        ("s-{" . projectile-previous-project-buffer)
        ("C-c {" . projectile-previous-project-buffer)))

(use-package counsel-projectile
  :defer 1
  :after (counsel projectile)
  :custom
  (counsel-projectile-remove-current-buffer t)
  (counsel-projectile-remove-current-project t)
  (compilation-scroll-output t)
  :commands
  counsel-projectile-mode
  :config
  ;; When switching projects, go straight to dired in the project root.
  (setf (car counsel-projectile-switch-project-action) 4)
  (counsel-projectile-mode)
  :bind
  (:map projectile-mode-map
        ("s-p" . counsel-projectile)
        ("s-P" . counsel-projectile-switch-project)
        ("s-r" . counsel-imenu)
        ("M-s-f" . counsel-projectile-rg)
        ("s-b" . counsel-projectile-switch-to-buffer)))

(use-package counsel-etags
  :defer 5
  :after counsel
  :custom
  ;; TODO: Get this working with Clojure (ctags parses namespaces but
  ;; `counsel-etags-find-tag-at-point' doesn't. Wouldn't this be `clojure-mode's
  ;; responsibility? I'm pretty sure it keys off of sexp
  (tags-revert-without-query t)
  ;; Don't warn when TAGS files are large.
  (large-file-warning-threshold nil)
  :hook
  ;; Incrementally update TAGS file when the file is saved.
  (prog-mode-hook
   . (lambda ()
       (add-hook 'after-save-hook
                 'counsel-etags-virtual-update-tags 'append 'local)))
  ((js-mode-hook js2-mode-hook) . counsel-etags-setup-smart-rules)
  :commands
  (counsel-etags-find-tag-at-point counsel-etags-scan-code counsel-etags-list-tag))

(use-package company
  :defer 1
  :custom
  (company-dabbrev-ignore-case t)
  :commands
  global-company-mode
  :config
  (global-company-mode)
  ;; :hook
  ;; TODO: Figure out how to make company-mode work in the minibuffer.
  ;; (minibuffer-setup . company-mode)
  :bind
  (("M-/" . company-complete)
   :map company-active-map
   ("RET" . nil)
   ("<return>" . nil)
   ("<tab>" . company-complete-selection)
   ("C-s" . company-filter-candidates)
   ("M-?" . company-complete-selection)
   ("M-." . company-show-location)
   :map minibuffer-local-map
   ("M-/" . completion-at-point)
   :map minibuffer-local-completion-map
   ("M-/" . completion-at-point)))

(use-package prescient
  :defer 1
  :commands
  prescient-persist-mode
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :defer 1
  :after (prescient ivy)
  :commands
  ivy-prescient-mode
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :defer 1
  :after (prescient company)
  :commands
  company-prescient-mode
  :config
  (company-prescient-mode))

(use-package dumb-jump
  :defer 5
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'rg)
  :hook
  (prog-mode-hook . dumb-jump-mode)
  ;; dumb-jump shadows some Eshell key bindings, and is not useful there anyway
  (eshell-mode . (lambda () (dumb-jump-mode -1)))
  :bind
  (:map dumb-jump-mode-map
        ("s-j" . dumb-jump-go-prompt)
        ("s-." . dumb-jump-go)
        ("s-J" . dumb-jump-quick-look)))

;; (use-package spotlight.el
;;   :git "https://github.com/cjp/spotlight.el.git"
;;   :commands
;;   (spotlight spotlight-fast)
;;   :bind
;;   ("C-c M-s" . spotlight)
;;   ("C-c M-S" . spotlight-fast))

(bind-key "s-5" #'replace-regexp-entire-buffer)

(provide 'm-search)

;;; m-search.el ends here
