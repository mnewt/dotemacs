;;; m-library.el --- Emacs Lisp Libraries -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton

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

(require 'seq)
(require 'subr-x)
(use-package dash :demand t)
(use-package s :demand t)
(use-package f :demand t)
(use-package shut-up
  :commands shut-up
  :config
  (defun shut-up-advice-wrapper (symbol &rest args)
    "Wrap the call to function named SYMBOL with `shut-up'."
    (shut-up (apply symbol args)))

  (defmacro shut-up-advice-add (symbol)
    "Advise the function named SYMBOL to shut up."
    `(advice-add ,symbol :around #'shut-up-advice-wrapper))

  (defmacro shut-up-advice-add (symbol)
    "Remove shut up advice for the function named SYMBOL."
    `(advice-remove ,symbol #'shut-up-advice-wrapper)))

(provide 'm-library)

;;; m-library.el ends here
