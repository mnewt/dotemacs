;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hydra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lv)

(require 'ibuffer)

(require 'hideshow)

(require 'ediff)

(use-package hydra
  :commands
  (hydra-default-pre hydra-keyboard-quit hydra--call-interactively-remap-maybe
                     hydra-show-hint hydra-set-transient-map))

(defhydra hydra-multiple-cursors (:hint nil)
  "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil)
  ("<mouse-1>" mc/add-cursor-on-click)
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore))

(defhydra hydra-outline (:color pink :hint nil)
  "
Outline

^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_s_ sublevels     _a_ all         _u_ up
_t_ body          _e_ entry       _n_ next visible
_o_ other         _i_ children    _p_ previous visible
_c_ entry         _k_ branches    _f_ forward same level
_l_ leaves        _s_ subtree     _b_ backward same level
_d_ subtree

"
  ;; Hide
  ("q" outline-hide-sublevels)    ; Hide everything but the top-level headings
  ("t" outline-hide-body)         ; Hide everything but headings (all body lines)
  ("o" outline-hide-other)        ; Hide other branches
  ("c" outline-hide-entry)        ; Hide this entry's body
  ("l" outline-hide-leaves)       ; Hide body lines in this entry and sub-entries
  ("d" outline-hide-subtree)      ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" outline-show-all)          ; Show (expand) everything
  ("e" outline-show-entry)        ; Show this heading's body
  ("i" outline-show-children)     ; Show this heading's immediate child sub-headings
  ("k" outline-show-branches)     ; Show all sub-headings under this heading
  ("s" outline-show-subtree)      ; Show (expand) everything in this heading & below
  ;; Move
  ("u" outline-up-heading)                ; Up
  ("n" outline-next-visible-heading)      ; Next
  ("p" outline-previous-visible-heading)  ; Previous
  ("f" outline-forward-same-level)        ; Forward - same level
  ("b" outline-backward-same-level)       ; Backward - same level
  ("q" nil "leave"))

(defhydra hydra-hs (:color pink :hint nil)
  "
Hideshow

Hide^^            ^Show^            ^Toggle^    ^Navigation^
----------------------------------------------------------------
_h_ hide all      _s_ show all      _t_ toggle    _n_ next line
_d_ hide block    _a_ show block                _p_ previous line
_l_ hide level

_q_ quit
"
  ("s" hs-show-all)
  ("h" hs-hide-all)
  ("a" hs-show-block)
  ("d" hs-hide-block)
  ("t" hs-toggle-hiding)
  ("l" hs-hide-level)
  ("n" forward-line)
  ("p" (forward-line -1))
  ("q" nil))

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

;; (advice-add 'occur-mode-goto-occurrence :after #'other-window-hydra-occur)

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

(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_ view         _m_ mark             _(_ details        _i_ insert-subdir  _W_  wdired
_C_ copy           _O_ view other   _U_ unmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_ delete         _o_ open other   _u_ unmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ rename         _M_ chmod        _t_ toggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_ extension mark   _s_ sort           _=_ pdiff
_S_ symlink        ^ ^              _F_ find marked      _._ toggle hydra   \\ flyspell
_r_ rsync          ^ ^              ^ ^                  ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

_q_ quit
"
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ;; ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or single directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-rsync)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("W" wdired-change-to-wdired-mode)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
^Mark^         ^Actions^         ^View^          ^Select^              ^Navigation^
_m_ mark      _D_ delete       _g_ refresh    _q_ quit             _k_   ↑    _h_
_u_ unmark    _s_ save marked  _S_ sort       _TAB_ toggle         _RET_ visit
_*_ specific  _a_ all actions  _/_ filter     _o_ other window     _j_   ↓    _l_
_t_ toggle    _._ toggle hydra _H_ help       C-o other win no-select
"
  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)
  ("t" ibuffer-toggle-marks)

  ("D" ibuffer-do-delete)
  ("s" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("S" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)
  ("H" describe-mode :color blue)

  ("h" ibuffer-backward-filter-group)
  ("k" ibuffer-backward-line)
  ("l" ibuffer-forward-filter-group)
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)

  ("TAB" ibuffer-toggle-filter-group)

  ("o" ibuffer-visit-buffer-other-window :color blue)
  ("q" quit-window :color blue)
  ("." nil :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                                     :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                       :after-exit
                                       (if (eq major-mode 'ibuffer-mode)
                                           (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(require 'ibuffer)

(bind-keys (("C-c C-m" . hydra-multiple-cursors/body)
            ("C-c #" . hydra-outline/body)
            ("C-c @" . hydra-hs/body)
            ("C-c C-o" . hydra-occur-dwim/body)
            :map dired-mode-map
            ("." . hydra-dired/body)
            :map ibuffer-mode-map
            ("." . hydra-ibuffer-main/body)))
