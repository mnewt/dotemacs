;;; vc.el --- Version Control  -*- lexical-binding: t -*-

;;; Commentary:

;; Version Control customization

;;; Code:

(defun git-add-current-file (file)
  "Run `git add' on the FILE visited in the current buffer."
  (interactive (list (buffer-file-name)))
  (let ((dir (s-trim (shell-command-to-string "git rev-parse --show-toplevel"))))
    (if (= 0 (call-process-shell-command (concat "git add " file)))
        (message "File %s was added to the git repo at %s." (buffer-file-name) dir)
      (error "Failed to add file %s to the git repo at %s" (buffer-file-name) dir))))

(defun dired-git-add ()
  "Run `git add' on the selected files in a dired buffer."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (message "> git add %s" files)
    (dired-do-shell-command "git add" nil files)
    (dired-revert)))

(defvar git-home-repo-dir
  (expand-file-name "repos" (or (getenv "XDG_CONFIG_HOME") "~/.config")))

(defun git-worktree-link (gitdir worktree)
  "Link git WORKTREE at GITDIR.
https://github.com/magit/magit/issues/460#issuecomment-36139308"
  (interactive (list (read-directory-name "Gitdir: ")
                     (read-directory-name "Worktree: ")))
  (with-temp-file (expand-file-name ".git" worktree)
    (insert "gitdir: " (file-relative-name gitdir worktree) "\n"))
  (magit-call-git "config" "-f" (expand-file-name "config" gitdir)
                  "core.worktree" (file-relative-name worktree gitdir))
  ;; Configure projectile to only look at tracked files
  (if (boundp 'projectile-git-command)
      (setq projectile-git-command "git ls-files -zc --exclude-standard")))

(defun git-worktree-unlink (worktree)
  "Unlink git WORKTREE at GITDIR."
  (interactive (list (read-directory-name "Worktree: ")))
  ;; Configure projectile back to default, which looks for all non-ignored files
  (if (boundp 'projectile-git-command)
      (setq projectile-git-command "git ls-files -zco --exclude-standard"))
  ;; This does `git config --unset core.worktree'.  We don't actually
  ;; have to do this and not doing it would have some advantages, but
  ;; might be confusing.
  ;; (magit-set nil "core.worktree")
  ;; This causes an error if this actually is a directory, which is
  ;; a good thing, it saves us from having to do this explicitly :-)
  (delete-file (expand-file-name ".git" worktree)))

(defun git-home-link (repo)
  "Interactively link a git REPO's worktree to $HOME."
  (interactive (list (completing-read "Link git home repository: "
                                      (directory-files git-home-repo-dir nil "^[^.]")
                                      nil t)))
  (setq repo (expand-file-name repo git-home-repo-dir))
  (git-worktree-link repo (getenv "HOME"))
  (message "Linked repo at %s" repo))

(defun git-home-unlink ()
  "Unlink the current git repo's worktree from $HOME."
  (interactive)
  (let ((f (expand-file-name ".git" (getenv "HOME"))))
    (git-worktree-unlink (getenv "HOME"))
    (message "Unlinked repo at %s" f)))

(with-eval-after-load 'vc
  ;; VC follows the link and visits the real file, telling you about it in the
  ;; echo area.
  (setq vc-follow-symlinks t
        ;; Backup even if file is in vc.
        vc-make-backup-files t))

(use-package magit
  :custom
  (magit-repository-directories `((,code-directory . 1)))
  (magit-completing-read-function 'ivy-completing-read)
  :config
  (use-package forge :demand t)

  :commands
  magit-call-git
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch))

(use-package magit-todos
  :commands
  magit-todos--scan-with-git-grep
  :custom
  (magit-todos-scanner #'magit-todos--scan-with-git-grep)
  :hook
  (magit-mode-hook . magit-todos-mode))

(use-package git-timemachine
  :bind
  ("C-x t" . git-timemachine))

(use-package gist
  :commands
  gist-list)

(use-package diff-hl
  :commands
  (diff-hl-magit-post-refresh diff-hl-mode diff-hl-dired-mode)
  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  ((prog-mode-hook markdown-mode-hook) . diff-hl-mode)
  (dired-mode-hook . diff-hl-dired-mode))

(use-package smerge-mode
  :config
  (defhydra hydra-smerge (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  
  :bind
  (:map smerge-mode-map
        ("C-s-s" . hydra-smerge/body)))

(bind-keys
 ("M-m l" . git-home-link)
 ("M-m u" . git-home-unlink)
 ("C-x G" . projectile-git-ls-files-dired)
 :map m-file-map
 (";" . git-add-current-file))

(provide 'm-vc)

;;; vc.el ends here
