;;; dired.el --- My dired configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Loads dired customizations and related packages.

;;; Code:

;; Sometimes (depending on how it's compiled and/or where the binary is?)
;; `auto-compression-mode' doesn't load quite right, and then `find-library' and
;; friends can't locate elisp source. Setting up the mode explicitly seems to
;; fix it.

(use-package jka-cmpr-hook
  :defer 1
  :ensure nil
  :commands
  auto-compression-mode
  :config
  (auto-compression-mode))

(defun upsearch (filename &optional dir)
  "Recursively search up a directory tree for FILENAME.

Start search in DIR or `default-directory'."
  (let ((dir (or dir default-directory)))
    (while (not (or (string= "/" dir)
                    (member filename (directory-files dir))))
      (setq dir (file-name-directory (directory-file-name dir))))
    (unless (string= "/" dir) dir)))

;;;; psync

(defvar-local psync-directory nil
  "Cached directory for `psync'.

It is always buffer local.")
(make-variable-buffer-local 'psync-directory)

(defun psync-maybe ()
  "If we find a `psync_config' file then run `psync'."
  (interactive)
  (when-let
      ((default-directory
         (or psync-directory
             (and default-directory
                  (not (file-remote-p default-directory))
                  (let ((d (shell-command-to-string "git rev-parse --show-toplevel")))
                    (when (and (string-prefix-p "fatal: not a git repository" d)
                               (file-exists-p (expand-file-name "psync_config" d)))
                      d))))))
    (setq psync-directory default-directory)
    (if (= 0 (call-process-shell-command "psync"))
        (message "psync in directory %s finished." default-directory)
      (error "Synchronization with psync failed in directory: %s" default-directory))))

(add-hook 'after-save-hook #'psync-maybe)

;;;; File utils

(defun dos2unix ()
  "Convert DOS line endings to Unix ones."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t)))
  (set-buffer-file-coding-system 'unix 't))

(defun unix2dos ()
  "Convert Unix encoded buffer to DOS encoding.
https://edivad.wordpress.com/2007/04/03/emacs-convert-dos-to-unix-and-vice-versa/"
  (interactive)
  (set-buffer-file-coding-system 'dos))

(defvar touch-history nil
  "History for `touch' command.")

(defun touch (cmd)
  "Run `touch CMD' in `default-directory'."
  (interactive (list (read-shell-command "Run touch (like this): "
                                         "touch "
                                         'touch-history
                                         "touch ")))
  (shell-command cmd))

(defun tail (file)
  "Run `tail -f' on FILE.
Tries to find a file at point."
  (interactive (list (completing-read "Tail file: "
                                      'read-file-name-internal
                                      'file-exists-p t nil 'file-name-history
                                      (thing-at-point 'filename))))
  (let ((cmd (if prefix-arg
                 (read-shell-command "Run tail (like this): "
                                     "tail -f"
                                     'tail-history)
               "tail -f")))
    (async-shell-command (format "%s %s" cmd file))))

(defun df ()
  "Display the local host's disk usage in human readable form."
  (interactive)
  (print (shell-command-to-string "df -h")))

;;;; OS program interaction

(defun reveal-in-windows-explorer (file)
  "Reveal FILE in Windows Explorer."
  (call-process "explorer" nil 0 nil
                (concat "/select," (dired-replace-in-string "/" "\\" file))))

(defun os-reveal-file (&optional file)
  "Reveal FILE using the operating system's GUI file browser."
  (interactive)
  (let ((file (or file buffer-file-name)))
    (message "Revealing %s..." file)
    (pcase system-type
      ('darwin (progn
                 (use-package reveal-in-osx-finder
                   :if (eq system-type 'darwin)
                   :commands
                   (reveal-in-osx-finder))
                 (reveal-in-osx-finder file)))
      ('windows-nt (reveal-in-windows-explorer file))
      ('cygwin (reveal-in-windows-explorer file)))))

(defun os-open-file (&optional file)
  "Open visited FILE in default external program.
When in dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
  (interactive)
  (let* ((file (if (eq major-mode 'dired-mode)
                   (dired-get-file-for-visit)
                 (or file buffer-file-name)))
         (open (pcase system-type
                 ('darwin "open")
                 ((or 'gnu 'gnu/linux 'gnu/kfreebsd) "xdg-open")
                 ((or 'windows-nt 'cygwin) "command")))
         (program (if (or current-prefix-arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (message "Opening %s in the OS registered external program..." file)
    (call-process program nil 0 nil file)))

;;;; Dired

(defun dired-open-file ()
  "Open file at point in OS default program."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (os-open-file file)))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-aFhl")
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-dwim-target t)
  ;; `dired-omit-mode' is managed by `dired-filter'.
  ;; (dired-omit-mode t)
  (dired-omit-files "\\`\\(?:[#.]\\|flycheck_\\).*")
  ;; Try to use GNU ls on macOS since BSD ls doesn't explicitly support
  ;; Emacs and can run into issues with certain characters in the file name.
  (insert-directory-program (or (executable-find "gls"
                                                 (executable-find "ls"))))
  ;; Don't prompt to kill buffers of deleted directories.
  (find-ls-option '("-print0 | xargs -0 ls -alhd" . ""))
  :commands
  dired-summary
  dired-do-delete
  dired-mark
  dired-display-file
  dired-find-file-other-window
  dired-sort-toggle-or-edit
  dired-toggle-marks
  dired-unmark-all-marks
  dired-unmark
  dired-view-file
  dired-ediff-files
  :config
  (defhydra hydra-dired (:hint nil :color pink)
    "
_+_ mkdir          _v_ view         _m_ mark             _(_ details        _i_ insert-subdir
_C_ copy           _O_ view other   _U_ unmark all       _)_ omit-mode      _W_  wdired
_D_ delete         _o_ open other   _u_ unmark           _l_ redisplay      _w_ kill-subdir
_R_ rename         _M_ chmod        _t_ toggle           _g_ revert buf     _e_ ediff
_Y_ rel symlink    _G_ chgrp        _E_ extension mark   _s_ sort           _r_ rsync
_S_ symlink        _z_ compress     _F_ find marked                       _?_ summary
_A_ find regexp    _Q_ repl regexp                                      _q_ quit

C-x C-q : edit     C-c C-c : commit C-c ESC : abort                 _._ toggle hydra
"
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("?" dired-summary)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy) ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer) ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay) ;; relist the marked or single directory
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
    ("v" dired-view-file) ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("W" wdired-change-to-wdired-mode)
    ("Y" dired-do-relsymlink)
    ("z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  (dired-mode-hook . (lambda ()
                       (unless (file-remote-p default-directory)
                         (auto-revert-mode))))
  :bind
  (:map dired-mode-map
        ("." . hydra-dired/body)
        ("C-c C-o" . dired-open-file)
        ("T" . touch)
        ("F" . tail-file)
        (";" . dired-git-add)))

(use-package dired-x
  :ensure nil
  :after dired
  :defer 6
  :config
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  :bind
  (:map dired-mode-map
        ("C-." . dired-omit-mode)))

(use-package dired-rsync
  :bind
  (:map dired-mode-map
        ("C-c C-r" . dired-rsync)))

(use-package disk-usage
  :bind
  (:map dired-mode-map
        (")" . disk-usage-here)
        ("C-)" . disk-usage)))

(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t)
  :commands
  (wdired-change-to-wdired-mode)
  :bind
  (:map dired-mode-map
        ("C-c C-p" . wdired-change-to-wdired-mode)))

(use-package dired-hacks-utils
  :defer 5
  :commands
  dired-utils-format-information-line-mode
  :config
  (dired-utils-format-information-line-mode))

(use-package dired-rainbow
  :defer 5
  :after dired-hacks-utils
  :config
  (dired-rainbow-define-chmod directory "#0074d9" "d.*" 'end)
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml") 'end)
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata") 'end)
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx" "xls" "xlsx" "vsd" "vsdx") 'end)
  (dired-rainbow-define markdown "#4dc0b5" ("org" "org_archive" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt") 'end)
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc") 'end)
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac") 'end)
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg") 'end)
  (dired-rainbow-define log "#c17d11" ("log" "log.1" "log.2" "log.3" "log.4" "log.5" "log.6" "log.7" "log.8" "log.9") 'end)
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "fish" "sed" "sh" "zsh" "vim") 'end)
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "hy" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "cljc" "cljx" "edn" "scala" "js" "jsx") 'end)
  (dired-rainbow-define compiled "#6cb2eb" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" "java") 'end)
  (dired-rainbow-define executable "#8cc4ff" ("com" "exe" "msi") 'end)
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar") 'end)
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp") 'end)
  (dired-rainbow-define encrypted "#f2d024" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem") 'end)
  (dired-rainbow-define fonts "#f6993f" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf") 'end)
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak") 'end)
  (dired-rainbow-define vc "#6cb2eb" ("git" "gitignore" "gitattributes" "gitmodules") 'end)
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*" 'end)
  (dired-rainbow-define junk "#7F7D7D" ("DS_Store" "projectile") 'end))

(use-package dired-rainbow-x
  :defer 5
  :after dired-rainbow
  :ensure nil
  :commands
  dired-rainbow-listing-mode
  :config
  (dired-rainbow-listing-mode))

(use-package dired-filter
  :disabled t
  :after dired-hacks-utils
  :custom
  (dired-filter-verbose nil)
  :hook
  (dired-mode-hook . dired-filter-mode))

(use-package dired-list
  :defer 5
  :after dired-hacks-utils
  :git (:uri "https://github.com/Fuco1/dired-hacks"
             :files "dired-list.el")
  :commands
  (dired-list
   dired-list-git-ls-files
   dired-list-locate
   dired-list-find-file
   dired-list-find-name
   dired-list-grep))

(use-package dired-subtree
   :after dired-hacks-utils
   :bind
   (:map dired-mode-map
         ("I" . dired-subtree-cycle)
         ("TAB" . dired-subtree-cycle)
         ("C-, i" . dired-subtree-insert)
         ("C-, r" . dired-subtree-remove)
         ("C-, R" . dired-subtree-revert)
         ("C-, n" . dired-subtree-narrow)
         ("C-, ^" . dired-subtree-up)
         ("C-, v" . dired-subtree-down)))

(use-package dired-collapse
  :after dired-hacks-utils
  :hook
  (dired-mode-hook . dired-collapse-mode))

(use-package dired-sidebar
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  :hook
  (dired-sidebar-mode-hook . (lambda ()
                               (unless (file-remote-p default-directory)
                                 (auto-revert-mode))))
  :bind
  ("C-x C-d" . dired-sidebar-toggle-sidebar))

(defun dired-list-init-files ()
  "List Emacs init files."
  (interactive)
  (git-home-link "dotemacs")
  (dired-list-git-ls-files user-emacs-directory)
  (when (bound-and-true-p dired-omit-mode) (dired-omit-mode -1)))
  
(defun dired-list-dotfiles ()
  "List Emacs init files."
  (interactive)
  (git-home-link "dotfiles")
  (dired-list-git-ls-files "~"))

(use-package counsel-tramp
  :hook
  (counsel-tramp-pre-command-hook . (lambda () (projectile-mode 0)))
  (counsel-tramp-quit-hook . projectile-mode)
  :commands
  counsel-tramp)

(bind-keys
 ("C-x M-s" . psync-maybe)
 ("C-c o" . os-open-file)
 ("C-c O" . os-reveal-file))

(provide 'm-file)

;;; file.el ends here
