;;; Init.el --- Emacs init file --- -*- lexical-binding: t -*-

;;; Commentary:
;; It's an Emacs init file. Relies on heavily on use-package for its
;; organization and performance features.

;;; Code:

(defconst emacs-start-time (current-time))

(defvar elisp-directory "~/.emacs.d/lisp")
"Local elisp configuration files go here."

(dolist-with-progress-reporter
    (p
     '(start
       package
       library
       environment
       persist
       private
       appearance
       help
       navigate
       search
       file
       net
       vc
       edit
       shell
       eshell
       notes
       lisp
       modes)

     (progn
       (defconst emacs-load-time
         (float-time (time-subtract (current-time) emacs-start-time)))
       (message "Emacs loaded packages in %.3f seconds." emacs-load-time)
       emacs-load-time))
    "Emacs is starting... "
  (load-file (format "%s/m-%s.el" elisp-directory (symbol-name p))))

(provide 'init)

;;; init.el ends here
