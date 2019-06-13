;;; m-help.el --- Help and Documentation -*- lexical-binding: t -*-

;;; Commentary:

;; Help and Documentation lookup

;;; Code:

(with-eval-after-load 'simple
  (setq suggest-key-bindings 5))

(use-package paradox
  :commands paradox-list-packages)

(use-package help-at-pt
  :custom
  (help-at-pt-display-when-idle t)
  :config
  (help-at-pt-set-timer))

;; (use-package help-fns+)

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

(use-package shr
  :defer t
  :custom
  (shr-color-visible-luminance-min 60)
  (shr-color-visible-distance-min 5)
  (shr-use-colors nil))

(add-hook 'after-init-hook #'global-eldoc-mode)

(add-hook 'after-init-hook #'which-func-mode)

(use-package which-key
  :defer t
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
  :load-path "src/eg.el"
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
  (defun dash-docs-update-all-docsets ()
    "Update all docsets."
    (interactive)
    (seq-doseq (d (dash-docs-installed-docsets))
      (when (memq d (dash-docs-official-docsets))
        (dash-docs-install-docset d))))

  :custom
  (dash-docs-docsets-path "~/.config/docsets")
  (dash-docs-browser-func #'eww)
  (dash-docs-common-docsets (dash-docs-installed-docsets))
  (dash-docs-enable-debugging nil)
  :commands
  (counsel-dash counsel-dash-at-point counsel-dash-install-docset dash-docs-installed-docsets dash-docs-official-docsets)
  :bind
  ("M-s-l" . counsel-dash)
  ("C-h C-d" . counsel-dash)
  ("M-s-." . counsel-dash-at-point))

(use-package hydra
  :config
  (use-package lv)

  (defun hydra-move-splitter-left (arg)
    "Move window splitter left by ARG characters."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right by ARG characters."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up by ARG characters."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down by ARG characters."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
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
    "Move"
    ("n" next-line)
    ("p" previous-line)
    ("f" forward-char)
    ("b" backward-char)
    ("a" beginning-of-line)
    ("e" move-end-of-line)
    ("v" scroll-up-command)
    ;; Converting M-v to V here by analogy.
    ("V" scroll-down-command)
    ("l" recenter-top-bottom))

  :commands
  (defhydra hydra-default-pre hydra-keyboard-quit
    hydra--call-interactively-remap-maybe hydra-show-hint
    hydra-set-transient-map
    hydra-window/body)
  :bind
  ("C-s-v" . hydra-move/body)
  ("C-c w" . hydra-window/body))

(use-package replace
  :ensure nil
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

(bind-keys
 ("C-h C-i" . elisp-index-search)
 ("C-h M-i" . info-apropos)
 :map Info-mode-map
 ("j" . next-line)
 ("k" . previous-line)
 ("J" . next-line-4)
 ("K" . previous-line-4))

(seq-doseq (m '(Info-mode-map help-mode-map))
  (bind-keys :map (symbol-value m)
             ("j" . next-line)
             ("k" . previous-line)
             ("J" . next-line-4)
             ("K" . previous-line-4)))

(provide 'm-help)

;;; m-help.el ends here
