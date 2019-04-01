;;; 1pass.el -- tiny wrapper to get usernames & passwords from 1pass
;;
;; Copyright (C) 2017 David Creemer, (twitter: @dcreemer)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Commentary:
;;
;; Thin wrapper around the 1pass CLI utility.  It is likely that bugs will be
;; found, so please report any findings as issues or pull requests here:
;; https://github.com/dcreemer/1pass

;;; Code:
(require 's)

(defvar 1pass-cli-executable
  (executable-find "1pass")
  "Path to the 1pass executable.")

;; private helpers
(defun 1pass--cli-run (item field)
  "Call 1pass with given ITEM and FIELD."
  (with-temp-buffer
    (let* ((exit-code
            (apply #'call-process
                   (list 1pass-cli-executable nil t nil "-p" item field))))
      (if (zerop exit-code)
          (s-chomp (buffer-string))
        (error (s-chomp (buffer-string)))))))

;; public API
(defun 1pass-field-for (item field)
  "Lookup ITEM in 1pass and return the data from the given FIELD, if any."
  (1pass--cli-run item field))

(defun 1pass-password-for (item)
  "Lookup ITEM in 1pass and return the password, if any."
  (1pass--cli-run item "password"))

(defun 1pass-username-for (item)
  "Lookup ITEM in 1pass and return the username, if any."
  (1pass--cli-run item "username"))

;; deprecated API:
(define-obsolete-function-alias '1pass--item-field    '1pass-field-for)
(define-obsolete-function-alias '1pass--item-password '1pass-password-for)
(define-obsolete-function-alias '1pass--item-username '1pass-username-for)

(provide '1pass)

;;; 1pass.el ends here
