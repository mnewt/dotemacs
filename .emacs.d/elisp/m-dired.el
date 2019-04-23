;;; m-dired.el --- My dired configuration -*- lexical-binding: t -*-

;; Author: Matthew Newton
;; Maintainer: Matthew Newton
;; Version: version
;; Package-Requires: (emacs "24.1")
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

;; Loads dired customizations and related packages.

;;; Code:

(require 'dired-x)

;; (use-package dired+
;;   :commands
;;   (dired-get-filename))

;; (use-package find-dired+)

(use-package ivy-dired-history
  :config
  (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)
  :bind
  (:map dired-mode-map
        ("," . dired)))

(setq dired-listing-switches "-aFhl"
      dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-dwim-target t
      ;; dired-omit-mode t
      ;; dired-omit-files "\\`[#.].*"
      ;; Try to use GNU ls on macOS since BSD ls doesn't explicitly support
      ;; Emacs and can run into issues with certain characters in the file name.
      insert-directory-program (or (executable-find "gls")
                                   (executable-find "ls"))
      ;; Don't prompt to kill buffers of deleted directories.
      dired-clean-confirm-killing-deleted-buffers nil
      find-ls-option '("-print0 | xargs -0 ls -alhd" . ""))

(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t)
  :commands
  (wdired-change-to-wdired-mode)
  :bind
  ("C-c C-p" . wdired-change-to-wdired-mode))

(use-package diredfl
  :hook
  (after-init . diredfl-mode))

(use-package dired-rainbow
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#f2d024" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#f2d024" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    (dired-rainbow-define microsoft "#3465a4" ("doc" "docx" "xls" "xlsx" "vsd" "vsdx"))))

(use-package dired-filter
  :commands
  (dired-filter-by-name
   dired-filter-by-regexp
   dired-filter-by-extension
   dired-filter-by-dot-files
   dired-filter-by-omit
   dired-filter-by-garbage
   dired-filter-by-predicate
   dired-filter-by-file
   dired-filter-by-directory
   dired-filter-by-mode
   dired-filter-by-symlink
   dired-filter-by-executable))

;; (use-package dired-list
;;   :commands
;;   (dired-list-git-ls-files
;;    dired-list-locate
;;    dired-list-find-file
;;    dired-list-find-name
;;    dired-list-grep))

(use-package dired-subtree
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
  :hook
  (dired-mode . dired-collapse-mode))

(use-package dired-rsync
  :bind
  (:map dired-mode-map
        ("C-c C-r" . dired-rsync)))

(use-package dired-sidebar
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  :bind
  (("C-x d" . dired-sidebar-toggle-sidebar)))

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

(defun ls-lisp-format-file-size (file-size &optional human-readable level)
  (setq level (or level 1000))
  (if (or (not human-readable)
          (< file-size 1024))
      (format (if (floatp file-size) " %11.0f" " %11d") file-size)
    (cl-do ((file-size (/ file-size 1024.0) (/ file-size 1024.0))
            ;; kilo, mega, giga, tera, peta, exa
            (post-fixes (list "k" "M" "G" "T" "P" "E") (cdr post-fixes))
            (l level (1- l)))
        ((or (= 0 l)
             (< file-size 1024)) (format " %10.0f%s" file-size (car post-fixes))))))

(defun dired-format-summary-line ()
  "Format the `total used in directory' and `available' space as
human readable."
  (save-excursion
    (goto-char (point-min))
    (forward-line)
    (let ((inhibit-read-only t)
          (limit (line-end-position)))
      (while (re-search-forward "\\(directory\\|available\\) \\(\\<[0-9]+\\>\\)" nil t)
        (replace-match (save-match-data
                         (propertize (string-trim
                                      (ls-lisp-format-file-size
                                       (* 1024 (string-to-number (match-string 2))) t))
                                     'invisible 'dired-hide-details-information))
                       t nil nil 2)))))

(defun dired-format-summary-line ()
  "Format the `total used in directory' and `available' space as
human readable."
  (save-excursion
    (goto-char (point-min))
    (forward-line)
    (let ((inhibit-read-only t)
          (kill-ring kill-ring)
          (limit (line-end-position)))
      (while (re-search-forward "^\\s-*total used in directory [0-9BGKM]+ available \\([0-9]+\\)$"
                                nil t)
        (forward-char)
        (unless (dired-utils-get-filename)
          (let* ((avail (or (word-at-point) "0"))
                 (avail-hr (s-trim (ls-lisp-format-file-size (* 1024 (string-to-number avail)) t 1))))
            (kill-word 1)
            (insert (propertize avail-hr 'invisible 'dired-hide-details-information))))))))

(add-hook 'dired-after-readin-hook #'dired-make-available-human-readable)

(bind-keys
 :map dired-mode-map
 ("C-c o" . dired-open-file)
 ("T" . touch)
 ("C-." . dired-omit-mode))

(provide 'm-dired)

;;; m-dired.el ends here
