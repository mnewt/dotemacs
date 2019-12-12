;;; log-mode.el --- Log File Mode -*- lexical-binding: t -*-

;; Author: Matthew Newton
;; Maintainer: Matthew Newton
;; Version: 0.1
;; Package-Requires: ()
;; Homepage: https://github.com/mnewt/log-mode
;; Keywords: log


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

;; Major mode for viewing log files

;;; Code:

(defvar log-mode-hook nil)

(defvar log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ?k #'keep-lines)
    (define-key map ?f #'flush-lines)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.log\\'" . log-mode))

(defconst log-font-lock-keywords-1
  `((,(regexp-opt '("error" "ERROR")) . '(:foreground "red"))
    (,(regexp-opt '("WARNING" "warn" "WARN" "warning")) . '(:foreground "yellow"))
    (,(regexp-opt '("info" "INFO" "information" "INFORMATION")) . '(:foreground "white"))
    (,(regexp-opt '("debug" "DEBUG")) . '(:foreground "cyan"))
    (,(regexp-opt '("trace" "TRACE")) . '(:foreground "purple"))))

(defconst log-font-lock-keywords-2
  (append log-font-lock-keywords-1
          (list ("\<[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}T?[0-9]\{2\}:[0-9]\{2\}:[0-9]\{2\}[.0-9]*?Z\>" . font-lock-comment-face))))

(defvar log-font-lock-keywords log-font-lock-keywords-2)

;;;###autoload
(define-derived-mode log-mode fundamental-mode "Log"
  "Major mode for viewing log files."
  (set (make-local-variable 'font-lock-defaults '(log-font-lock-keywords))))

(provide 'log-mode)

;;; log-mode.el ends here
