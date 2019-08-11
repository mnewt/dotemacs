;;; m-help.el --- Help and Documentation -*- lexical-binding: t -*-

;;; Commentary:

;; Help and Documentation lookup

;;; Code:

;; Change yes/no prompts to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show key bindings when the command is executed from `M-x'.
(with-eval-after-load 'simple
  (setq suggest-key-bindings 5))

;; Enable all commands without silly warnings.
(with-eval-after-load 'novice
  (setq disabled-command-function nil))

(use-package help-at-pt
  :defer 2
  :custom
  (help-at-pt-display-when-idle t)
  :commands
  help-at-pt-set-timer
  :config
  (help-at-pt-set-timer))

(use-package goto-addr
  :hook
  ((prog-mode-hook text-mode-hook) . goto-address-mode))

(use-package helpful
  :bind
  ("C-h ." . helpful-at-point)
  ("C-h f" . helpful-callable)
  ("C-h c" . helpful-command)
  ("C-h F" . helpful-function)
  ("C-h k" . helpful-key)
  ("C-h M" . helpful-macro)
  ("C-h M-s" . helpful-symbol)
  ("C-h v" . helpful-variable))

(with-eval-after-load 'shr
  (eval-when-compile
    (defvar shr-color-visible-luminance-min)
    (defvar shr-color-visible-distance-min)
    (defvar shr-use-colors))
  (setq shr-color-visible-luminance-min 60
        shr-color-visible-distance-min 5
        shr-use-colors nil))

(use-package eldoc
  :defer 2
  :commands
  eldoc-add-command
  global-eldoc-mode
  :config
  (eldoc-add-command #'company-select-next
                     #'company-select-previous
                     #'keyboard-quit
                     #'outshine-self-insert-command)
  (global-eldoc-mode))

(use-package which-key
  :demand t
  :defer 2
  :commands
  which-key-mode
  :config
  (which-key-mode)
  :bind
  ("M-s-h" . which-key-show-top-level))

(use-package man
  :custom
  ;; Make the manpage the current buffer in the other window
  (Man-notify-method 'aggressive)
  :config
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)
  :bind
  ("C-h M-m" . man))

(defun tramp-aware-woman (man-page-path)
  "Open a remote man page at MAN-PAGE-PATH via TRAMP."
  (interactive)
  (let ((dir default-directory))
    (woman-find-file
     (if (file-remote-p dir)
         (let ((vec (tramp-dissect-file-name dir)))
           (tramp-make-tramp-file-name
            (tramp-file-name-method vec)
            (tramp-file-name-user vec)
            (tramp-file-name-host vec)
            man-page-path))
       man-page-path))))

(use-package eg
  :straight (eg :type git :host github :repo "mnewt/eg.el")
  :ensure-system-package
  (eg . "pip install eg")
  :bind
  ("C-h e" . eg))

(defvar dash-docs-docsets-path "~/.config/docsets"
  "Local path to save docsets.")

(defun dash-docs-installed-docsets ()
  "Return a list of the currently installed docsets."
  (mapcar (lambda (f) (string-trim-right f ".docset"))
          (directory-files dash-docs-docsets-path nil "[^.]*\.docset")))

(use-package counsel-dash
  :ensure-system-package sqlite3
  :init
  ;; counsel-dash calls 'remove-duplicates, which is no longer available in
  ;; Emacs master.
  (defalias 'remove-duplicates 'cl-remove-duplicates)

  (defun dash-docs-update-docsets-var (&optional _)
    "Update `dash-docs-common-docsets' variable."
    (interactive (list t))
    (setq dash-docs-common-docsets (dash-docs-installed-docsets)))

  (advice-add 'dash-docs-install-docset :after #'dash-docs-update-docsets-var)
  (advice-add 'dash-docs-install-user-docset :after #'dash-docs-update-docsets-var)

  (defun dash-docs-update-all-docsets ()
    "Update all official and unofficial docsets."
    (interactive)
    (seq-doseq (d (dash-docs-installed-docsets))
      (cond
       ((memq d (dash-docs-official-docsets))
        (dash-docs-install-docset d))
       ((memq d (dash-docs-unofficial-docsets))
        (dash-docs-install-user-docset d))
       (t
        (message "Skipping manually installed docset: %s..." d))))
    (dash-docs-update-docsets-var))

  (defun eww-other-window (url)
    "Fetch URL and render the page.

Open the `eww' buffer in another window."
    (require 'eww)
    (interactive
     (let* ((uris (eww-suggested-uris))
            (prompt (concat "Enter URL or keywords"
                            (if uris (format " (default %s)" (car uris)) "")
                            ": ")))
       (list (read-string prompt nil 'eww-prompt-history uris))))
    (switch-to-buffer-other-window (current-buffer))
    (eww url t))

  :custom
  (dash-docs-docsets-path "~/.config/docsets")
  (dash-docs-browser-func #'eww-other-window)
  (dash-docs-common-docsets (dash-docs-installed-docsets))
  (dash-docs-enable-debugging nil)
  :commands
  (counsel-dash counsel-dash-at-point dash-docs-install-docset
                dash-docs-official-docsets dash-docs-unofficial-docsets)
  :bind
  ("M-s-l" . counsel-dash)
  ("C-h C-d" . counsel-dash)
  ("M-s-." . counsel-dash-at-point))

(use-package lv
  :commands
  lv-message
  lv-window
  lv-delete-window)

(use-package hydra
  :defer 2
  :config
  (defun hydra-move-splitter-left (arg)
    "Move window splitter left by ARG characters."
    (interactive "p")
    (if (windmove-find-other-window 'right)
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right by ARG characters."
    (interactive "p")
    (if (windmove-find-other-window 'right)
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up by ARG characters."
    (interactive "p")
    (if (windmove-find-other-window 'up)
        (enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down by ARG characters."
    (interactive "p")
    (if (windmove-find-other-window 'up)
        (shrink-window arg)
      (enlarge-window arg)))

  (defhydra hydra-window (:hint nil)
    "
MOVE WINDOW^   _h_ left          _j_ down                    _k_ up             _l_ right
MOVE BUFFER^   _←_ left          _↓_ down                    _↑_ up             _→_ right
SPLIT^         _V_ vertical      _H_ horizontal              _u_ undo           _r_ redo
SIZE^          _b_ thinner       _n_ taller                  _p_ shorter        _f_ wider                 _B_ balance
DELETE^        _d_ kill buffer   _D_ kill buffer and window  _w_ delete window  _W_ delete other windows
              _q_ quit
"
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("<left>" buf-move-left)
    ("<down>" buf-move-down)
    ("<up>" buf-move-up)
    ("<right>" buf-move-right)
    ("V" (lambda () (interactive) (split-window-right) (windmove-right)))
    ("H" (lambda () (interactive) (split-window-below) (windmove-down)))
    ("u" (progn (winner-undo) (setq this-command 'winner-undo)))
    ("r" winner-redo)
    ("b" hydra-move-splitter-left)
    ("n" hydra-move-splitter-down)
    ("p" hydra-move-splitter-up)
    ("f" hydra-move-splitter-right)
    ("B" balance-windows)
    ("d" kill-current-buffer)
    ("D" kill-buffer-and-window)
    ("w" delete-window)
    ("W" delete-other-windows)
    ("q" nil))

  (defhydra hydra-move (:hint nil)
    "
hydra-move: [_n_ _N_ _p_ _P_ _v_ _V_ _u_ _d_] [_f_ _F_ _b_ _B_ _a_ _A_ _e_ _E_] [_,_ _._ _l_ _c_] _q_"
    ("n" next-line)
    ("N" scroll-down-margin)
    ("p" previous-line)
    ("P" scroll-up-margin)
    ("v" scroll-up-command)
    ("V" scroll-down-command)
    ("u" scroll-window-up)
    ("d" scroll-window-down)
    ("f" forward-char)
    ("F" forward-word)
    ("b" backward-char)
    ("B" backward-word)
    ("a" mwim-beginning-of-code-or-line)
    ("A" beginning-of-defun)
    ("e" mwim-end-of-code-or-line)
    ("E" end-of-defun)
    ("," beginning-of-buffer)
    ("." end-of-buffer)
    ("l" recenter-top-bottom)
    ("c" goto-last-change)
    ("q" nil))

  :commands
  (defhydra hydra-default-pre hydra-keyboard-quit
    hydra--call-interactively-remap-maybe hydra-show-hint
    hydra-set-transient-map
    hydra-window/body)
  :bind
  ("C-s-v" . hydra-move/body)
  ("C-c w" . hydra-window/body))

(use-package replace
  :straight nil
  :commands
  occur-next
  occur-prev
  :config
  (defun occur-dwim ()
    "Call `occur' with a sane default, chosen as the thing under point or selected region."
    (interactive)
    (push (if (region-active-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))
            (let ((sym (thing-at-point 'symbol)))
              (when (stringp sym)
                (regexp-quote sym))))
          regexp-history)
    (call-interactively 'occur))

  (declare-function other-window-hydra-occur 'hydra)

  (advice-add 'occur-mode-goto-occurrence :after #'other-window-hydra-occur)

  ;; Focus on *Occur* window right away.
  (add-hook 'occur-hook (lambda () (other-window 1)))

  (defun reattach-occur ()
    "Switch to Occur buffer and launch the hydra."
    (if (get-buffer "*Occur*")
        (switch-to-buffer-other-window "*Occur*")
      (hydra-occur-dwim/body)))

  ;; Used in conjunction with occur-mode-goto-occurrence-advice this helps keep
  ;; focus on the *Occur* window and hides upon request in case needed later.
  (defhydra hydra-occur-dwim ()
    "Occur mode"
    ("o" occur-dwim "Start occur-dwim" :color red)
    ("j" occur-next "Next" :color red)
    ("k" occur-prev "Prev":color red)
    ("h" delete-window "Hide" :color blue)
    ("r" (reattach-occur) "Re-attach" :color red)
    ("q" nil))

  :bind
  (:map occur-mode-map
        ("C-o" . hydra-occur-dwim/body)))

(use-package paradox
  :custom
  (paradox-automatically-star t)
  :commands paradox-list-packages)

(bind-keys
 ("C-h C-i" . elisp-index-search)
 ("C-h M-i" . info-apropos)
 :map Info-mode-map
 ("j" . next-line)
 ("k" . previous-line))

(seq-doseq (m '(Info-mode-map help-mode-map))
  (bind-keys :map (symbol-value m)
             ("j" . next-line)
             ("k" . previous-line)))

(provide 'm-help)

;;; m-help.el ends here
