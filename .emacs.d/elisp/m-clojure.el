;;; m-clojure.el --- My Clojure config -*- lexical-binding: t -*-

;; Author: Matthew Newton
;; Maintainer: Matthew Newton
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


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

(require 'm-lisp)

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

;;;###autoload
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

(provide 'm-clojure)

;;; m-clojure.el ends here
