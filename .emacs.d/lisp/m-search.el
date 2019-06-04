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

(defun counsel--call-in-other-window-action (x)
  "Switch to other window and call command X."
  (switch-to-buffer-other-window (current-buffer)
                                 (call-interactively (intern x))))

(defun counsel-rg-default-directory (f &rest args)
  "Call F (`counsel-rg') with ARGS from `default-directory'.

It seems like `counsel-rg' should call itself from
`default-directory' without assistance but in my experience it
looks for a project root directory instead. If we want to search
from the project root, we can use `counsel-projectile-rg'."
  (let ((initial-input (car args))
        (initial-directory (or (cadr args) default-directory))
        (extra-rg-args (caddr args))
        (rg-prompt (or (cadddr args) (format "(%s) rg: " default-directory))))
    (funcall f initial-input initial-directory extra-rg-args rg-prompt)))

;; For byte compiler
(declare-function projectile-project-root 'projectile)

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

;; Show line in the original buffer from occur mode
(setq list-matching-lines-jump-to-current-line t)

(use-package re-builder
  :custom
  ;; string syntax means you don't need to double escape things.
  (reb-re-syntax 'string)
  :bind
  ("C-c r" . re-builder))

(use-package pcre2el
  :hook
  ((emacs-lisp-mode lisp-interaction-mode reb-mode) . rxt-mode))

(use-package wgrep
  :custom
  ;; Save changed buffers immediately when exiting wgrep mode
  (wgrep-auto-save-buffer t)
  :bind
  (:map grep-mode-map
        ("C-c C-p" . wgrep-change-to-wgrep-mode)
        :map occur-mode-map
        ("C-c C-p" . wgrep-change-to-wgrep-mode)))

(use-package rg
  :ensure-system-package
  (rg . ripgrep)
  :custom
  (rg-keymap-prefix (kbd "C-c M-s"))
  :after
  (wgrep-ag)
  :config
  (rg-enable-default-bindings (kbd "C-r"))
  :hook
  (rg-mode . wgrep-ag-setup))

;; (use-package counsel-etags
;;   :custom
;;   ;; TODO: Get this working with Clojure (ctags parses namespaces but
;;   ;; `counsel-etags-find-tag-at-point' doesn't. Wouldn't this be `clojure-mode's
;;   ;; responsibility? I'm pretty sure it keys off of sexp
;;   (tags-revert-without-query t)
;;   ;; Don't warn when TAGS files are large.
;;   (large-file-warning-threshold nil)
;;   :hook
;;   ;; Incrementally update TAGS file when the file is saved.
;;   (prog-mode . (lambda ()
;;                  (add-hook 'after-save-hook
;;                            'counsel-etags-virtual-update-tags 'append 'local)))
;;   :commands
;;   (counsel-etags-find-tag-at-point counsel-etags-scan-code counsel-etags-list-tag))

(use-package company
  :custom
  (company-dabbrev-ignore-case t)
  :hook
  (after-init . global-company-mode)
  :bind
  (([remap dabbrev-expand] . company-complete)
   :map company-active-map
   ("RET" . nil)
   ("<return>" . nil)
   ("C-e" . company-complete-selection)
   ("M-." . company-show-location)))

(use-package ivy
  :custom
  (enable-recursive-minibuffers t)
  (ivy-display-style 'fancy)
  (ivy-count-format "[%d/%d] ")
  :hook
  (after-init . ivy-mode)
  :commands
  (ivy--reset-state ivy-add-actions)
  :bind
  (:map ivy-mode-map
        ("C-c C-r" . ivy-resume)
        :map ivy-minibuffer-map
        ("C-e" . ivy-partial-or-done)))

;; Hydra requirement
(use-package lv)

(use-package ivy-hydra
  :after lv
  :defer 1)

(use-package swiper
  :bind
  (:map ivy-minibuffer-map
        ("C-c C-c" . ivy-toggle-calling)
        ("s-5" . ivy--replace-regexp-entire-buffer)))

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

;; (defun counsel-register-action-delete (register)
;;   "Delete the REGISTER."
;;   (let ((val (get-text-property 0 'register register)))
;;     (setq register-alist (delq (assoc val register-alist) register-alist))
;;     (message "Deleted register %s." (single-key-description val))))

;; (defun counsel-register-action-then-delete (register)
;;   "Perform the default action on REGISTER, then delete it."
;;   (counsel-register-action register)
;;   (counsel-register-action-delete register))

(use-package counsel
  :custom
  (counsel-find-file-at-point t)
  (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :config
  (ivy-add-actions
   'counsel-M-x
   `(("j" counsel--call-in-other-window-action "other window")))
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
   '(("f"
      ivy--find-file-action
      "find file")
     ("j"
      ivy--switch-buffer-other-window-action
      "other window")
     ("k"
      ivy--kill-buffer-action
      "kill")
     ("r"
      ivy--rename-buffer-action
      "rename")))
  (ivy-set-actions
   'counsel-register
   '(("d" counsel-register-action-delete "delete")
     ("k" counsel-register-action-then-delete "call then delete")))
  :hook
  (after-init . counsel-mode)
  :bind
  (:map counsel-mode-map
        ("M-x" . counsel-M-x)
        ("C-h C-k" . counsel-descbinds)
        ("s-f" . counsel-grep-or-swiper)
        ("s-F" . counsel-rg)
        ("C-x C-f" . counsel-find-file)
        ("C-x f" . counsel-recentf)
        ("C-x j" . counsel-file-jump)
        ("s-b" . counsel-switch-buffer)
        ("s-B" . counsel-switch-buffer-other-window)
        ("C-h <tab>" . counsel-info-lookup-symbol)
        ("C-h C-a" . counsel-apropos)
        ("C-c u" . counsel-unicode-char)
        ("C-c g" . counsel-git)
        ("C-c j" . counsel-git-grep)
        ("C-c M-o" . counsel-outline)
        ("M-s-v" . counsel-yank-pop)
        ("M-Y" . counsel-yank-pop)
        ;; Don't shadow default binding.
        ([remap yank-pop] . nil)
        ("M-Y" . counsel-yank-pop)
        ([remap find-file] . counsel-find-file))
  (:map ivy-minibuffer-map
        ("M-y" . ivy-next-line-and-call)
        ("C-c C-v" . counsel-git-grep-preview-toggle))
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuffer-history)))

(defcustom counsel-git-grep-preview t
  "When non-nil, display the file and match immediately.
This works for `counsel-git-grep', `counsel-ag', and
  derivatives."
  :type 'boolean
  :group 'counsel)

(defun counsel-git-grep-preview-toggle ()
  "Toggle `counsel-git-grep-preview'."
  (if counsel-git-grep-preview
      (setq counsel-git-grep-preview nil)
    (setq counsel-git-grep-preview t)))

(defvar counsel--git-grep-temporary-buffers nil
  "Internal. Track open buffers during `counsel-git-grep' session.")

(defvar counsel--git-grep-previous-buffers nil
  "Internal. Used to restore buffer order after `counsel-git-grep'.")

(defun counsel--git-grep-unwind ()
  "Clear temporary file buffers and restore `buffer-list'.
The buffers are those opened during a session of `counsel-git-grep'."
  (mapc #'kill-buffer counsel--git-grep-temporary-buffers)
  (mapc #'bury-buffer (cl-remove-if-not #'buffer-live-p counsel--git-grep-previous-buffers))
  (setq counsel--git-grep-temporary-buffers nil
        counsel--git-grep-previous-buffers nil)
  (counsel-delete-process)
  (swiper--cleanup))

(defvar ivy-text)

(defun counsel--line (x)
  "Go to line number X in the current file."
  (swiper--cleanup)
  (goto-char (point-min))
  (forward-line (1- (string-to-number x)))
  (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
  (swiper--add-overlays (ivy--regex ivy-text)))

(defun counsel--git-grep-update-fn ()
  "Display the current selection and its buffer."
  (let ((current (ivy-state-current ivy-last)))
    (when (and counsel-git-grep-preview
               (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" current))
      (unless counsel--git-grep-previous-buffers
        (setq counsel--git-grep-previous-buffers (buffer-list)))
      (let* ((file-name (match-string-no-properties 1 current))
             (line-number (match-string-no-properties 2 current))
             (buffer (or (cl-some (lambda (b)
                                    (when (string= (buffer-file-name b) file-name)
                                      b))
                                  (buffer-list))
                         (let ((buffer (find-file-noselect file-name)))
                           (cl-pushnew buffer counsel--git-grep-temporary-buffers)
                           buffer))))
        (with-ivy-window (pop-to-buffer-same-window buffer)
                         (counsel--line line-number))))))

(defun counsel-git-grep (&optional cmd initial-input)
  "Grep for a string in the current Git repository.
When CMD is a string, use it as a \"git grep\" command.
When CMD is non-nil, prompt for a specific \"git grep\" command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive "P")
  (let ((proj-and-cmd (counsel--git-grep-cmd-and-proj cmd))
        proj)
    (setq proj (car proj-and-cmd))
    (setq counsel-git-grep-cmd (cdr proj-and-cmd))
    (counsel-require-program counsel-git-grep-cmd)
    (let ((collection-function
           (if proj
               #'counsel-git-grep-proj-function
             #'counsel-git-grep-function))
          (default-directory (if proj
                                 (car proj)
                               (counsel-locate-git-root))))
      (ivy-read "git grep: " collection-function
                :initial-input initial-input
                :dynamic-collection t
                :keymap counsel-git-grep-map
                :action #'counsel-git-grep-action
                :history 'counsel-git-grep-history
                :caller 'counsel-git-grep
                :unwind #'counsel--git-grep-unwind
                :update-fn #'counsel--git-grep-update-fn))))

(with-eval-after-load 'ivy
  (cl-pushnew 'counsel-git-grep ivy-highlight-grep-commands))

(cl-defun counsel-ag (&optional initial-input initial-directory extra-ag-args ag-prompt
                                &key caller)
  "Grep for a string in the current directory using ag.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS string, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.
CALLER is passed to `ivy-read'."
  (interactive)
  (setq counsel-ag-command counsel-ag-base-command)
  (setq counsel--regex-look-around counsel--grep-tool-look-around)
  (counsel-require-program counsel-ag-command)
  (when current-prefix-arg
    (setq initial-directory
          (or initial-directory
              (read-directory-name (concat
                                    (car (split-string counsel-ag-command))
                                    " in directory: "))))
    (setq extra-ag-args
          (or extra-ag-args
              (read-from-minibuffer (format
                                     "%s args: "
                                     (car (split-string counsel-ag-command)))))))
  (setq counsel-ag-command (counsel--format-ag-command (or extra-ag-args "") "%s"))
  (let ((default-directory (or initial-directory
                               (counsel--git-root)
                               default-directory)))
    (ivy-read (or ag-prompt
                  (concat (car (split-string counsel-ag-command)) ": "))
              #'counsel-ag-function
              :initial-input initial-input
              :dynamic-collection t
              :keymap counsel-ag-map
              :history 'counsel-git-grep-history
              :action #'counsel-git-grep-action
              :caller (or caller 'counsel-ag)
              :unwind #'counsel--git-grep-unwind
              :update-fn #'counsel--git-grep-update-fn)))

(advice-add 'counsel-rg :around #'counsel-rg-default-directory)

(use-package prescient
  :hook
  (after-init . prescient-persist-mode))

(use-package ivy-prescient
  :hook
  (after-init . ivy-prescient-mode))

(use-package company-prescient
  :hook
  (after-init . company-prescient-mode))

(use-package projectile
  :custom
  (projectile-keymap-prefix (kbd "C-c p"))
  (projectile-completion-system 'ivy)
  (projectile-project-search-path (list code-directory))
  (projectile-globally-ignored-files '("TAGS" "package-lock.json"))
  (projectile-switch-project-action 'projectile-dired)
  :config
  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm start"
                                    :test "npm test"
                                    :test-suffix ".test")
  (projectile-register-project-type 'clojure-cli '("deps.edn")
                                    :compile "clj "
                                    :test-suffix "_test")
  :hook
  (projectile-after-switch-project . projectile-load-settings)
  (after-init . projectile-mode)
  :commands
  (projectile-register-project-type)
  :bind
  (:map projectile-mode-map
        ("s-}" . projectile-next-project-buffer)
        ("C-c }" . projectile-next-project-buffer)
        ("s-{" . projectile-previous-project-buffer)
        ("C-c {" . projectile-previous-project-buffer)))

(use-package counsel-projectile
  :custom
  (counsel-projectile-remove-current-buffer t)
  (counsel-projectile-remove-current-project t)
  (compilation-scroll-output t)
  :config
  ;; When switching projects, go straight to dired in the project root.
  (setf (car counsel-projectile-switch-project-action) 4)
  :hook
  (after-init . counsel-projectile-mode)
  :bind
  ("s-p" . counsel-projectile)
  ("s-P" . counsel-projectile-switch-project)
  ("s-r" . counsel-imenu)
  ("M-s-f" . counsel-projectile-rg))

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'rg)
  :hook
  (prog-mode . dumb-jump-mode)
  ;; dumb-jump shadows some Eshell key bindings, and is not useful there anyway
  (eshell-mode . (lambda () (dumb-jump-mode -1)))
  :bind
  (:map dumb-jump-mode-map
        ("s-j" . dumb-jump-go-prompt)
        ("s-." . dumb-jump-go)
        ("s-J" . dumb-jump-quick-look)))

(use-package flash-thing
  :straight
  (:type git :host github :repo "mnewt/flash-thing"))
  ;; :hook
  ;; (after-init . flash-thing-mode))

(use-package spotlight
  :straight
  (:type git :host github :repo "cjp/spotlight.el")
  :bind
  ("")
  :commands
  (spotlight spotlight-fast))

(bind-key "s-5" #'replace-regexp-entire-buffer)

(provide 'm-search)

;;; m-search.el ends here
