;;; m-mail.el --- My mail config -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton
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

;; My mail config

;;; Code:

(use-package mu4e
  :disabled
  :ensure nil
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :custom
  ;; Tell Emacs to send using `mu4e'
  (mail-user-agent 'mu4e-user-agent)
  ;; Don't save message to Sent Messages. The sending server saves the message.
  (mu4e-sent-messages-behavior 'delete)
  ;; Allow for updating mail using 'U' in the main view:
  (mu4e-get-mail-command "mbsync -aq")
  ;; Non-nil value retrieves mail and updates the database
  (mu4e-update-interval 1800)
  ;; Enable inline images in message view.
  (mu4e-view-show-images t)
  ;; Prevent duplicate UID issues with mbsync
  (mu4e-change-filenames-when-moving t)
  ;; Start with the first (default) context;
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  (mu4e-context-policy 'pick-first)
  ;; Use fancy unicode chars for marks and threads
  (mu4e-use-fancy-chars nil)
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-compose-dont-reply-to-self t)
  ;; convert org mode to HTML automatically
  (org-mu4e-convert-to-html t)
  (mu4e-headers-fields `((:human-date . 12)
                         (:flags . 5)
                         (:from-or-to . 20)
                         (:subject . nil)))
  (mu4e-view-show-images t)

  :config
  (use-package smtpmail
    :ensure nil
    :custom
    ;; Setting `smtp-queue-mail' to t prevents messages from being sent unless
    ;; they are explicitly flushed. nil is the default.
    ;; TODO: Queue messages and periodically flush them.
    (smtpmail-queue-mail nil)
    (send-mail-function 'async-smtpmail-send-it)
    (message-send-mail-function 'async-smtpmail-send-it)

    :config
    (defvar mail-queue-directory (expand-file-name "~/.mail/queue/")
      "Mail queue for SMTP outgoing mail messages.")
    (setq smtpmail-queue-dir (expand-file-name "cur" mail-queue-directory))

    (shell-command (format "mu mkdir %s" mail-queue-directory))
    (shell-command (format "touch %s" (expand-file-name ".noindex" mail-queue-directory))))


  (add-to-list 'mu4e-headers-actions
               '("Browse message" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions
               '("Browse message" . mu4e-action-view-in-browser) t)

  (defun org-capture-mu4e ()
    (interactive)
    "Capture a TODO item via email."
    (org-capture nil "m"))

  (use-package mu4e-maildirs-extension
    :config
    (mu4e-maildirs-extension))

  ;; Attachments from dired
  ;; http://www.djcbsoftware.nl/code/mu/mu4e/Dired.html#Dired

  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active-bg message buffers."
    (require 'gnus-dired)
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

  (setq gnus-dired-mail-mode 'mu4e-user-agent)

  (defvar mu4e-last-window nil
    "The last mu4e window.

Used by `mu4e-toggle'.")

  (defun mu4e-toggle (arg)
    "Toggle the display of `mu4e'."
    (interactive "P")
    (let ((r "*mu4e-"))
      (if (string-match-p r (buffer-name))
          (progn (setq mu4e-last-window (buffer-name))
                 (mapc #'bury-buffer (filter-buffers-by-name r))
                 (previous-buffer))
        (if arg
            (call-interactively #'mu4e)
          (if mu4e-last-window
              (switch-to-buffer mu4e-last-window)
            (mu4e-headers-search (mu4e-get-bookmark-query ?u)))))))

  (defun mu4e-update-mail-and-index-in-background (arg)
    "Run `mu4e-update-mail-and-index' in the background."
    (interactive "P")
    (mu4e-update-mail-and-index (not arg)))

  (defun mu4e-mark-execute-all-no-confirm (arg)
    "Run `mu4e-mark-execute-all' with no confirmation."
    (interactive "P")
    (mu4e-mark-execute-all (not arg)))

  (defun mbsync-start-imap-watch ()
    "Start the imap-watch daemon."
    (interactive)
    (call-process "imap-watch" nil (get-buffer-create "*imap-watch*")))

  (defhydra hydra-mu4e-headers (:color blue :hint nil)
    "
 ^General^   | ^Search^           | _!_ read    | _#_ deferred  | ^Switches^
-^^----------+-^^-----------------| _?_ unread  | _%_ pattern   |-^^------------------
 _n_ next    | _s_ search         | _r_ refile  | _&_ custom    | _O_ sorting
 _p_ prev    | _S_ edit prev qry  | _u_ unmk    | _+_ flag      | _P_ threading
 _]_ n unred | _/_ narrow search  | _U_ unmk *  | _-_ unflag    | _Q_ full-search
 _[_ p unred | _b_ search bkmk    | _d_ trash   | _T_ thr       | _V_ skip dups
 _y_ sw view | _B_ edit bkmk      | _D_ delete  | _t_ subthr    | _W_ include-related
 _R_ reply   | _{_ previous qry   | _m_ move    |-^^-------------+-^^------------------
 _C_ compose | _}_ next query     | _a_ action  | _|_ thru shl  | _`_ update, reindex
 _F_ forward | _C-+_ show more    | _A_ mk4actn | _H_ help      | _;_ context-switch
 _o_ org-cap | _C--_ show less    | _*_ *thing  | _q_ quit hdrs | _j_ jump2maildir"

    ;; general
    ("n" mu4e-headers-next)
    ("p" mu4e-headers-previous)
    ("[" mu4e-select-next-unread)
    ("]" mu4e-select-previous-unread)
    ("y" mu4e-select-other-view)
    ("R" mu4e-compose-reply)
    ("C" mu4e-compose-new)
    ("F" mu4e-compose-forward)
    ("o" org-capture-mu4e)

    ;; search
    ("s" mu4e-headers-search)
    ("S" mu4e-headers-search-edit)
    ("/" mu4e-headers-search-narrow)
    ("b" mu4e-headers-search-bookmark)
    ("B" mu4e-headers-search-bookmark-edit)
    ("{" mu4e-headers-query-prev)
    ("}" mu4e-headers-query-next)
    ("C-+" mu4e-headers-split-view-grow)
    ("C--" mu4e-headers-split-view-shrink)

    ;; mark stuff
    ("!" mu4e-headers-mark-for-read)
    ("?" mu4e-headers-mark-for-unread)
    ("r" mu4e-headers-mark-for-refile)
    ("u" mu4e-headers-mark-for-unmark)
    ("U" mu4e-mark-unmark-all)
    ("d" mu4e-headers-mark-for-trash)
    ("D" mu4e-headers-mark-for-delete)
    ("m" mu4e-headers-mark-for-move)
    ("a" mu4e-headers-action)
    ("A" mu4e-headers-mark-for-action)
    ("*" mu4e-headers-mark-for-something)

    ("#" mu4e-mark-resolve-deferred-marks)
    ("%" mu4e-headers-mark-pattern)
    ("&" mu4e-headers-mark-custom)
    ("+" mu4e-headers-mark-for-flag)
    ("-" mu4e-headers-mark-for-unflag)
    ("t" mu4e-headers-mark-subthread)
    ("T" mu4e-headers-mark-thread)

    ;; miscellany
    ("q" mu4e~headers-quit-buffer)
    ("H" mu4e-display-manual)
    ("|" mu4e-view-pipe)

    ;; switches
    ("O" mu4e-headers-change-sorting)
    ("P" mu4e-headers-toggle-threading)
    ("Q" mu4e-headers-toggle-full-search)
    ("V" mu4e-headers-toggle-skip-duplicates)
    ("W" mu4e-headers-toggle-include-related)

    ;; more miscellany
    ("`" mu4e-update-mail-and-index)
    (";" mu4e-context-switch)
    ("j" mu4e~headers-jump-to-maildir)

    ("." nil))

  :hook
  (mu4e-main-mode-hook . mu4e-maildirs-extension)
  (dired-mode-hook . turn-on-gnus-dired-mode)
  (mu4e-compose-mode-hook . turn-off-auto-fill)


  :bind
  ("s-m" . mu4e-toggle)
  ("M-m m" . mu4e)
  (:map mu4e-main-mode-map
        ("G" . mu4e-update-mail-and-index-in-background))
  (:map mu4e-headers-mode-map
        ("G" . mu4e-update-mail-and-index-in-background)
        ("x" . mu4e-mark-execute-all-no-confirm)
        ("." . hydra-mu4e-headers/body))
  (:map mu4e-view-mode-map
        ("<RET>" . mu4e~view-browse-url-from-binding)
        ("<tab>" . shr-next-link)
        ("<backtab>" . shr-previous-link)))

(provide 'm-mail)

;;; m-mail.el ends here
