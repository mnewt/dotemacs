;;; m-dired.el --- My dired configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Loads dired customizations and related packages.

;;; Code:

(require 'dired-x)

(use-package jka-cmpr-hook
  :straight (:type built-in)
  :hook
  ;; Sometimes (depending on how it's compiled and/or where the binary is?)
  ;; `auto-compression-mode' doesn't load quite right, and then `find-library'
  ;; and friends can't locate elisp source. Setting up the mode explicitly seems
  ;; to fix it.
  (after-init . auto-compression-mode))

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
    (if (= 0 (shell-command-exit-code "psync"))
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

(defun tail-file (file)
  "Run `tail -f' on FILE.
Tries to find a file at point."
  (interactive (list (completing-read "Tail file: "
                                      'read-file-name-internal
                                      'file-exists-p t nil 'file-name-history
                                      (thing-at-point 'filename))))
  (async-shell-command (concat "tail -f " file)))

(defun df ()
  "Display the local host's disk usage in human readable form."
  (interactive)
  (print (shell-command-to-string "df -h")))

(defun public-ip ()
  "Display the local host's apparent public IP address."
  (interactive)
  (message
   (with-current-buffer (url-retrieve-synchronously "https://diagnostic.opendns.com/myip")
     (goto-char (point-min))
     (re-search-forward "^$")
     (delete-char 1)
     (delete-region (point) (point-min))
     (buffer-string))))

(defun dis (hostname)
  "Resolve a HOSTNAME to its IP address."
  (interactive "MHostname: ")
  (message (shell-command-to-string
            (concat "drill "
                    hostname
                    " | awk '/;; ANSWER SECTION:/{flag=1;next}/;;/{flag=0}flag'"))))

;;;; OS program interaction

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  :commands
  (reveal-in-osx-finder))

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
      ('darwin (reveal-in-osx-finder file))
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

;;;; mnt

;; These functions execute the `mnt' utility, which uses config
;; profiles to mount smb shares (even through ssh tunnels).

(defun mnt-cmd (cmd)
  "Interactively Run a `mnt/umnt' utility (CMD).
The config is specified in the config file in `~/.mnt/'."
  (let ((config (completing-read (format "Run %s using config: " cmd)
                                 (directory-files "~/.mnt" nil "^[^.]")
                                 nil t)))
    (setq config (expand-file-name config "~/.mnt"))
    (if (async-shell-command (concat cmd " " config) "*mnt*")
        (message (format "%s succeeded with config file: %s" cmd config))
      (message (format "%s FAILED with config file: %s" cmd config)))))

(defun mnt ()
  "Mount a share using the `mnt' utility."
  (interactive)
  (mnt-cmd "sudo_mnt"))

(defun umnt ()
  "Unmount a share using the `umnt' utility."
  (interactive)
  (mnt-cmd "umnt"))

;;;; Dired

(use-package dired
  :straight
  (:type built-in)
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
  (dired-clean-confirm-killing-deleted-buffers nil)
  (find-ls-option '("-print0 | xargs -0 ls -alhd" . "")))

(use-package ivy-dired-history
  :config
  (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)
  :bind
  (:map dired-mode-map
        ("," . dired)))

(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t)
  :commands
  (wdired-change-to-wdired-mode)
  :bind
  (:map dired-mode-map
        ("C-c C-p" . wdired-change-to-wdired-mode)))

(use-package dired-efap
  :bind
  (:map dired-mode-map
        ("r" . dired-efap)))

(use-package dired-hacks-utils
  ;; :straight
  ;; (:type git :host github :repo "mnewt/dired-hacks" :files ("dired-hacks-utils.el"))
  :config
  :hook
  (after-init . dired-utils-format-information-line-mode))

(use-package dired-rainbow
  :straight
  (:type git :host github :repo "Fuco1/dired-hacks" :files ("dired-rainbow.el"))
  ;; :fork (:host github :repo "mnewt/dired-hacks"))
  :config
  (progn
    (defface dired-rainbow-permissions-face '((t (:inherit default)))
      "Face for Dired permissions."
      :group 'dired-rainbow)

    (defface dired-rainbow-inodes-face '((t (:inherit shadow)))
      "Face for Dired links."
      :group 'dired-rainbow)

    (defface dired-rainbow-user-face '((t (:inherit default)))
      "Face for Dired user."
      :group 'dired-rainbow)

    (defface dired-rainbow-group-face '((t (:inherit font-lock-comment-face)))
      "Face for Dired group."
      :group 'dired-rainbow)

    (defface dired-rainbow-size-face '((t (:inherit default)))
      "Face for Dired file size."
      :group 'dired-rainbow)

    (defface dired-rainbow-datetime-face
      '((((background dark)) (:inherit default :foreground "#999"))
        (t (:inherit default :foreground "#777")))
      "Face for Dired timestamp."
      :group 'dired-rainbow)

    (defface dired-rainbow-file-extension-face '((t (:inherit font-lock-comment-face)))
      "Face for Dired file extensions."
      :group 'dired-rainbow)

    (defface dired-rainbow-file-decoration-face '((t (:inherit default)))
      "Face for file decoration."
      :group 'dired-rainbow)

    (defface dired-rainbow-dash-face '((t (:inherit shadow)))
      "Face for file decoration."
      :group 'dired-rainbow)

    (defface dired-rainbow-filetype-directory-face '((t (:inherit font-lock-function-name-face)))
      "Face for file decoration."
      :group 'dired-rainbow)

    (defface dired-rainbow-filetype-link-face '((t (:inherit font-lock-string-face)))
      "Face for file decoration."
      :group 'dired-rainbow)

    (defcustom dired-rainbow-permissions-regexp "[-dl][-rwxlsStT]\\{9\\}[.+-@]?"
      "A regexp matching the permissions in the dired listing."
      :type 'string
      :group 'dired-rainbow)

    (defcustom dired-rainbow-inodes-regexp "[0-9]+"
      "A regexp matching the number of links in the dired listing."
      :type 'string
      :group 'dired-rainbow)

    (defcustom dired-rainbow-user-or-group-regexp "[a-z_][a-z0-9_-]*"
      "A regexp matching the user and group in the dired listing."
      :type 'string
      :group 'dired-rainbow)

    (defcustom dired-rainbow-size-regexp "[0-9.]+[kKmMgGtTpPi]\\{0,3\\}"
      "A regexp matching the file size in the dired listing."
      :type 'string
      :group 'dired-rainbow)

    (defcustom dired-rainbow-file-extension-regexp "\\.[^./]*?$"
      "A regexp matching file extensions."
      :type 'string
      :group 'dired-rainbow)

    (defcustom dired-rainbow-file-decoration-regexp
      "\\(?:[*/]\\| -> .*?\\(\\.*?\\)?\\)?"
      "A regexp matching the file decoration in the dired listing.

This is the `/', `*', or ` -> file_name' after the file name when
the `ls -F' option is used.

It should be wrapped in an optional capture group."
      :type 'string
      :group 'dired-rainbow)

    (defvar dired-rainbow-details-regexp
      (let ((sep "\\) +\\("))
        (concat "^ +\\("
                dired-rainbow-permissions-regexp sep
                dired-rainbow-inodes-regexp sep
                dired-rainbow-user-or-group-regexp sep
                dired-rainbow-user-or-group-regexp sep
                dired-rainbow-size-regexp sep
                dired-hacks-datetime-regexp
                "\\)")))

    (defvar dired-rainbow-ending-regexp
      (concat "\\("
              dired-rainbow-file-extension-regexp "\\)\\("
              dired-rainbow-file-decoration-regexp "\\)$"))

    (defvar dired-rainbow-listing-keywords
      `((,(concat "\\(total used in directory\\|available\\) +\\("
                  dired-rainbow-size-regexp "\\)")
         (1 'font-lock-comment-face)
         (2 'default))
        ("^ +\\(-\\)" 1 'dired-rainbow-dash-face)
        ("^ +\\(d\\)" 1 'dired-rainbow-filetype-directory-face)
        ("^ +\\(l\\)" 1 'dired-rainbow-filetype-link-face)
        ("^ +.\\(-\\)" 1 'dired-rainbow-dash-face)
        ("^ +..\\(-\\)" 1 'dired-rainbow-dash-face)
        ("^ +...\\(-\\)" 1 'dired-rainbow-dash-face)
        ("^ +....\\(-\\)" 1 'dired-rainbow-dash-face)
        ("^ +.....\\(-\\)" 1 'dired-rainbow-dash-face)
        ("^ +......\\(-\\)" 1 'dired-rainbow-dash-face)
        ("^ +.......\\(-\\)" 1 'dired-rainbow-dash-face)
        ("^ +........\\(-\\)" 1 'dired-rainbow-dash-face)
        ("^ +.........\\(-\\)" 1 'dired-rainbow-dash-face)
        (,dired-rainbow-details-regexp
         (1 'dired-rainbow-permissions-face)
         (2 'dired-rainbow-inodes-face)
         (3 'dired-rainbow-user-face)
         (4 'dired-rainbow-group-face)
         (5 'dired-rainbow-size-face)
         (6 'dired-rainbow-datetime-face))
        (,dired-rainbow-file-extension-regexp 0 'dired-rainbow-file-extension-face t)))

    (define-minor-mode dired-rainbow-listing-mode
      "Toggle highlighting of file listing details in Dired."
      :group 'dired-rainbow
      :lighter ""
      (progn
        (if dired-rainbow-listing-mode
            (font-lock-add-keywords 'dired-mode dired-rainbow-listing-keywords 'end)
          (font-lock-remove-keywords 'dired-mode dired-rainbow-listing-keywords))
        (mapc (lambda (b) (with-current-buffer b
                            (when (equal major-mode 'dired-mode)
                              (font-lock-refresh-defaults))))
              (buffer-list))))

    (dired-rainbow-listing-mode t)

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
    (dired-rainbow-define junk "#7F7D7D" ("DS_Store" "projectile") 'end)))

(use-package dired-filter
  ;; :straight
  ;; (:type git :host github :repo "mnewt/dired-hacks" :files ("dired-filter.el"))
  :custom
  (dired-filter-verbose nil)
  :hook
  (dired-mode . dired-filter-mode))

(use-package dired-list
  :straight
  (:type git :host github :repo "Fuco1/dired-hacks" :files ("dired-list.el"))
  :commands
  (dired-list-git-ls-files
   dired-list-locate
   dired-list-find-file
   dired-list-find-name
   dired-list-grep))

(use-package dired-subtree
  ;; :straight
  ;; (:type git :host github :repo "mnewt/dired-hacks" :files ("dired-subtree.el"))
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
  ;; :straight
  ;; (:type git :host github :repo "mnewt/dired-hacks" :files ("dired-collapse.el"))
  :hook
  (dired-mode . dired-collapse-mode))

(use-package dired-rsync
  :bind
  (:map dired-mode-map
        ("C-c C-r" . dired-rsync)))

(use-package dired-sidebar
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  :hook
  (dired-sidebar-mode . (lambda ()
                          (unless (file-remote-p default-directory)
                            (auto-revert-mode))))
  :bind
  (("C-x C-d" . dired-sidebar-toggle-sidebar)))

(use-package disk-usage
  :straight
  (:type git :host gitlab :repo "Ambrevar/emacs-disk-usage")
  :bind
  (:map dired-mode-map
        (")" . disk-usage-here)
        ("C-)" . disk-usage)))

(defun dired-open-file ()
  "Open file at point in OS default program."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (os-open-file file)))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(bind-keys
 ("C-x M-s" . psync-maybe)
 ("C-c o" . os-open-file)
 ("C-c O" . os-reveal-file)
 :map dired-mode-map
 ("C-c C-o" . dired-open-file)
 ("T" . touch)
 ("C-." . dired-omit-mode)
 ("F" . tail-file)
 (";" . dired-git-add))

(provide 'm-file)

;;; m-dired.el ends here
