;;; use-package-git.el --- Git extension for use-package -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (use-package "2.4"))
;; Homepage: https://github.com/mnewt/use-package-git
;; Keywords: dotemacs config package git


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

;; This package adds rudimentary support for installing packages directly from
;; git repositories.

;; It requires git 1.7.2.3 or higher.

;;; Code:

(require 'use-package-ensure)

(defcustom use-package-git-user-dir
  (expand-file-name "git" user-emacs-directory)
  "Directory containing the user's git packages."
  :group 'use-package-ensure
  :type 'string)

(defun use-package-ensure-git (name config)
  "Ensure that the git package NAME is cloned.

CONFIG is the PList supplied as the value to the :git key."
  (unless (and (file-exists-p use-package-git-user-dir)
               (car (file-attributes use-package-git-user-dir)))
    (make-directory use-package-git-user-dir t))
  (let ((dir (expand-file-name (plist-get config :dir) use-package-git-user-dir)))
    (unless (file-exists-p dir)
      ;; Clone the repo.
      (message "use-package-ensure-git is cloning package %s..." (symbol-name name))
      (shell-command (format "git -C %s clone %s %s"
                             use-package-git-user-dir
                             (plist-get config :uri)
                             dir))

      ;; Check out the ref (should work for branch, hash, or tag).
      ;; TODO: Create branch for tag?
      (when-let (ref (plist-get config :ref))
        (shell-command (format "git -C %s checkout %s" dir ref)))

      ;; Byte compile Elisp.
      (dolist (file (file-expand-wildcards (plist-get config :files)))
        (with-suppressed-warnings (byte-compile-file file))))
    (add-to-list 'load-path dir))
  name)

(defun use-package-ensure-dispatch (name args state &optional no-refresh)
  "Dispatch package NAME to the git and elpa ensure functions.))))))

ARGS, STATE, and NO-REFRESH are passed through."
  (unless (plist-get state :ensured)
    (use-package-ensure-elpa name args state no-refresh)))

;;;###autoload
(defun use-package-normalize/:git (name keyword args)
  "Normalize the :git property list for package NAME.

KEYWORD is the keyword that caused this function to be called
so... it's :git.

ARGS is a list of forms following the KEYWORD--in this case a
list of one."
  (use-package-only-one (symbol-name keyword) args
    #'(lambda (_label config)
        (cond
         ((stringp config)
          (list :dir (symbol-name name) :uri config :files "*.el"))

         ((and (listp config) (stringp (plist-get config :uri)))
          (setq config (plist-put config :dir (or (plist-get config :dir)
                                                  (symbol-name name))))
          (setq config (plist-put config :files (or (plist-get config :files)
                                                    "*.el")))
          config)

         (t
          (use-package-error
           (concat ":git wants either a string or a PList with a :uri key")))))))

;;;###autoload
(defun use-package-handler/:git (name _keyword config rest state)
  "Simply return `body' of package NAME.

STATE is updated to tell `use-package-ensure-dispatch' that this
package is already ensured and does not need to dispatch to
`ensure'.

CONFIG is the PList containing the git configuration.

NAME and REST are passed to `use-package-process-keywords'."
  (let* ((state (plist-put state :ensured t))
         (body (use-package-process-keywords name rest state)))
    ;; We use the same logic as `use-package-handler/:ensure'.
    (if (bound-and-true-p byte-compile-current-file)
        ;; Eval when byte-compiling,
        (use-package-ensure-git name config)
      ;;  or else wait until runtime.
      (push `(use-package-ensure-git ',name ',config) body))
    body))

(add-to-list 'use-package-keywords :git)

(setq use-package-ensure-function #'use-package-ensure-dispatch)

(provide 'use-package-git)

;;; use-package-git.el ends here
