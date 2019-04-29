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
                  (destructuring-bind (start . end) (bounds-of-thing-at-point 'sexp)
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

(use-package clojure-mode
  :config
  (add-to-list 'interpreter-mode-alist '("inlein" . clojure-mode))
  :hook
  ((clojure-mode clojurescript-mode) . turn-on-eldoc-mode))

;; (use-package clojure-mode-extra-font-locking
;;   :defer 1)

(defun toggle-nrepl-buffer ()
  "Toggle the nREPL REPL on and off"
  (interactive)
  (if (string-match "cider-repl" (buffer-name (current-buffer)))
      (delete-window)
    (cider-switch-to-repl-buffer)))

(defun cider-save-and-refresh ()
  (interactive)
  (save-buffer)
  (call-interactively 'cider-refresh))

(defun cider-eval-last-sexp-and-append ()
  (interactive)
  (cider-eval-last-sexp '(1)))

(use-package cider
  ;; :hook
  ;; (cider-repl-mode . (lambda () (company-mode nil)))
  :commands
  (cider-switch-to-repl-buffer)
  :bind
  (:map cider-mode-map
        ("s-<return>" . cider-eval-last-sexp)))

(defun reinstate-comint-simple-send ()
  "`inf-clojure' clobbers `comint-send-input' on all comint
buffers, not just `inf-clojure-mode' ones. This function
reinstates default behavior. See:
https://github.com/clojure-emacs/inf-clojure/issues/154"
  (unless (bound-and-true-p inf-clojure-minor-mode)
    (setq-local comint-input-sender 'comint-simple-send)))

(use-package inf-clojure
  :hook
  (comint-mode . reinstate-comint-simple-send)
  :bind
  (:map inf-clojure-minor-mode-map
        ("s-<return>" . inf-clojure-eval-last-sexp)
        ("C-c C-k" . inf-clojure-eval-buffer)))

;; (use-package sly
;;   ;; There are some problems building sly with straight.el in Windows
;;   :unless (eq system-type 'windows-nt)
;;   :custom
;;   (inferior-lisp-program (executable-find "sbcl"))
;;   :bind
;;   (:map sly-prefix-map
;;         ("M-h" . sly-documentation-lookup)))

;; (use-package sly-company
;;   :unless (eq system-type 'windows-nt)
;;   :hook
;;   (sly-mode . sly-company-mode)
;;   :config
;;   (add-to-list 'company-backends 'sly-company))

;; Configured to use CHICKEN Scheme
;; (use-package geiser
;;   :custom
;;   (geiser-default-implementation 'chicken)
;;   (geiser-mode-eval-last-sexp-to-buffer t)
;;   (scheme-program-name "csi -:c")
;;   :config
;;   (setq-default geiser-scheme-implementation 'chicken)

;;   ;; Indenting module body code at column 0
;;   (defun scheme-module-indent (state indent-point normal-indent) 0)
;;   (put 'module 'scheme-indent-function 'scheme-module-indent)
;;   (put 'and-let* 'scheme-indent-function 1)
;;   (put 'parameterize 'scheme-indent-function 1)
;;   (put 'handle-exceptions 'scheme-indent-function 1)
;;   (put 'when 'scheme-indent-function 1)
;;   (put 'unless 'scheme-indenfunction 1)
;;   (put 'match 'scheme-indent-function 1)
;;   :commands
;;   (geiser run-geiser run-chicken))

(provide 'm-lisp)

;;; m-lisp.el ends here
