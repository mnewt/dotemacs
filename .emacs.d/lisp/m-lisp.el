;;; m-lisp.el --- Lisp configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Lisp specific functionality

;;; Code:

(add-to-list 'auto-mode-alist '("Cask\\'" emacs-lisp-mode))

(defun eval-last-sexp-other-window (arg)
  "Run `eval-last-sexp' with ARG in the other window."
  (interactive "P")
  (save-window-excursion
    (other-window 1)
    (eval-last-sexp arg)))

(defun expression-to-register (register)
  "Interactively store an Emacs Lisp expression in a REGISTER.
If region is active, store that. Otherwise, store the sexp at
  point."
  (interactive (list (register-read-with-preview "Copy expression to register: ")))
  (set-register register
                (if (region-active-p)
                    (buffer-substring (mark) (point))
                  (cl-destructuring-bind
                      (start . end) (bounds-of-thing-at-point 'sexp)
                    (buffer-substring start end))))
  (setq deactivate-mark t)
  (when (called-interactively-p 'interactive) (indicate-copied-region)))

(defun eval-register (register)
  "Evaluate contents of register REGISTER as an Emacs Lisp expression.
REGISTER is a character and its contents are a string.

If called with a prefix arg, then insert the return value at
point.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (register-read-with-preview "Eval register: ")
                       current-prefix-arg)))
  (let* ((val (get-register register))
         (res (eval (car (read-from-string (format "(progn %s)" val))))))
    (when current-prefix-arg (register-val-insert res))))

(use-package clojure-mode
  :mode
  (("\\.clj\\'" . clojure-mode)
   ("\\.cljs\\'" . clojurescript-mode)
   ("\\.cljc\\'" . clojurec-mode))
  :interpreter
  ("inlein" . clojure-mode))

(use-package clojure-mode-extra-font-locking
  :after clojure-mode)

(use-package inf-clojure
  :commands
  inf-clojure
  inf-clojure-connect
  inf-clojure-minor-mode
  :config
  (defun inf-clojure-start-lumo ()
    "Start lumo as a subprocess and then connect to it over TCP.
This is preferable to starting it directly because lumo has lots
of problems in that context."
    (interactive)
    (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
    (inf-clojure-minor-mode)
    (shell-command "pkill -f 'lumo -d -n 2000'")
    (async-shell-command "lumo -d -n 2000")
    (run-with-idle-timer 2 nil (lambda () (inf-clojure-connect "localhost" 2000))))
  :bind
  (:map inf-clojure-minor-mode-map
        ("s-<return>" . inf-clojure-eval-last-sexp)
        ("C-c C-k" . inf-clojure-eval-buffer)))

(use-package cider
  :config
  (defun toggle-nrepl-buffer ()
    "Toggle the nREPL REPL on and off."
    (interactive)
    (if (string-match "cider-repl" (buffer-name (current-buffer)))
        (delete-window)
      (cider-switch-to-repl-buffer)))

  (defun cider-save-and-refresh ()
    "Save the buffer and refresh CIDER."
    (interactive)
    (save-buffer)
    (call-interactively 'cider-refresh))

  (defun cider-eval-last-sexp-and-append ()
    "Eval last sexp and append the result."
    (interactive)
    (cider-eval-last-sexp '(1)))

  :custom
  ;; Always prompt for the jack in command.
  (cider-edit-jack-in-command t)

  :commands
  (cider-jack-in cider-switch-to-repl-buffer)

  :hook
  ;; The standard advice function runs at the wrong time I guess? Anyway, it
  ;; often gets set to the wrong color when switching themes via `theme-choose'.
  (theme . (lambda () (when (fboundp 'cider-scale-background-color)
                        (setq cider-stacktrace-frames-background-color
                              (cider-scale-background-color)))))
  :bind
  (:map cider-mode-map
        ("s-<return>" . cider-eval-last-sexp)))

(use-package sly
  :custom
  (inferior-lisp-program (executable-find "sbcl"))
  :bind
  (:map sly-prefix-map
        ("M-h" . sly-documentation-lookup)))

(use-package scheme
  :config
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  
  (defun scheme-region-extend-function ()
    (when (not (get-text-property (point) 'font-lock-multiline))
      (let* ((heredoc nil)
             (new-beg
              (save-excursion
                (when (and (re-search-backward "#>\\|<#\\|#<[<#]\\(.*\\)$" nil t)
                           (not (get-text-property (point) 'font-lock-multiline)))
                  (let ((match (match-string 0))
                        (tag (match-string 1)))
                    (cond
                     ((equal match "#>") (point))
                     ((string-match-p "^#<[<#]" match) (setq heredoc tag) (point)))))))
             (new-end
              (save-excursion
                (if heredoc
                    (when (and (re-search-forward (concat "^" (regexp-quote heredoc) "$") nil t)
                               (not (get-text-property (point) 'font-lock-multiline)))
                      (point))
                  (when (and (re-search-forward "#>\\|<#" nil t)
                             (not (get-text-property (point) 'font-lock-multiline))
                             (equal (match-string 0) "<#"))
                    (point))))))
        (when (and new-beg new-end)
          (setq font-lock-beg new-beg)
          (setq font-lock-end new-end)
          (with-silent-modifications
            (put-text-property new-beg new-end 'font-lock-multiline t))
          (cons new-beg new-end)))))

  (defun scheme-syntax-propertize-foreign (_ end)
    (save-match-data
      (when (search-forward "<#" end t)
        (with-silent-modifications
          (put-text-property (1- (point)) (point)
                             'syntax-table (string-to-syntax "> cn"))))))

  (defun scheme-syntax-propertize-heredoc (_ end)
    (save-match-data
      (let ((tag (match-string 2)))
        (when (and tag (re-search-forward (concat "^" (regexp-quote tag) "$") nil t))
          (with-silent-modifications
            (put-text-property (1- (point)) (point)
                               'syntax-table (string-to-syntax "> cn")))))))

  (defun scheme-syntax-propertize (beg end)
    (goto-char beg)
    (scheme-syntax-propertize-sexp-comment (point) end)
    (funcall
     (syntax-propertize-rules
      ("\\(#\\);"
       (1 (prog1 "< cn" (scheme-syntax-propertize-sexp-comment (point) end))))
      ("\\(#\\)>"
       (1 (prog1 "< cn" (scheme-syntax-propertize-foreign (point) end))))
      ("\\(#\\)<[<#]\\(.*\\)$"
       (1 (prog1 "< cn" (scheme-syntax-propertize-heredoc (point) end)))))
     (point) end)))

(defun m-scheme-mode-setup ()
  "Set up `scheme-mode'.")
  
(add-hook 'scheme-mode-hook
          (lambda ()
            (setq font-lock-extend-region-functions
                  (cons 'scheme-region-extend-function
                        font-lock-extend-region-functions))))

(use-package geiser
  :custom
  (geiser-default-implementation 'chicken)
  (geiser-mode-eval-last-sexp-to-buffer t)
  (scheme-program-name "csi -:c")
  :config
  ;; Indenting module body code at column 0
  (defun scheme-module-indent (_state _indent-point _normal-indent) 0)
  (put 'module 'scheme-indent-function 'scheme-module-indent)
  (put 'and-let* 'scheme-indent-function 1)
  (put 'parameterize 'scheme-indent-function 1)
  (put 'handle-exceptions 'scheme-indent-function 1)
  (put 'when 'scheme-indent-function 1)
  (put 'unless 'scheme-indenfunction 1)
  (put 'match 'scheme-indent-function 1)
  :commands
  (geiser run-geiser run-chicken))

(bind-keys
 :map lisp-mode-shared-map
 ("s-<return>" . eval-last-sexp)
 ("C-s-<return>" . eval-last-sexp-other-window)
 ("C-c C-k" . eval-buffer)
 ("C-x C-r" . eval-region)
 ("C-x M-e" . pp-macroexpand-last-sexp)
 ("C-x r E" . expression-to-register)
 ("C-x r e" . eval-register))

(provide 'm-lisp)

;;; m-lisp.el ends here
