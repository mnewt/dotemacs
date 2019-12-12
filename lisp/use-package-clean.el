;; TODO Develop a better way to ensure only currently configured packages are
;; installed. Use `use-package-list'.
;; See https://yoo2080.wordpress.com/2014/05/16/how-to-list-emacs-package-dependencies/
(defun package-delete-all ()
  "Delete all packages in `package-user-dir'.

We do this to get rid of any stale packages and force a reinstall
on the next startup."
  (interactive)
  (shell-command (concat "rm -rf " package-user-dir)))

(defvar package-dependencies-alist nil
  "List of packages and their dependencies.")

(defun package-refresh-dependencies-alist ()
  "Refresh `package-dependencies-alist'."
  (setq package-dependencies-alist
        (cl-loop for pkg in package-activated-list
                 for pkg-vec = (cadr (assq pkg package-alist))
                 when pkg-vec
                 collect
                 (cons pkg
                       (cl-loop for req in (package-desc-reqs pkg-vec)
                                for req-name = (car req)
                                when (memq req-name package-activated-list)
                                collect req-name))))
  package-dependencies-alist)

(defun find-duplicates (list)
  "Get the duplicate elements from LIST."
  (cl-loop for (item . count) in
           (let ((counts '())
                 place)
             (dolist (el list)
               (setq place (assoc el counts))
               (if place
                   (cl-incf (cdr place))
                 (push (cons el 1) counts)))
             counts)
           if (> count 1)
           collect item))

(defun package-delete-unused ()
  "Delete unused packages."
  (interactive)
  (let* ((default-directory package-user-dir)
         ;; Could do this from `package-alist' / package-desc but what we really
         ;; care about is what is on disk, so go straight to it.
         installed-package-alist duplicates)
    (dolist (dir (file-expand-wildcards "*-*"))
      (when (file-directory-p dir)
        (push (cons (intern (replace-regexp-in-string "-[0-9\\.]+\\'" "" dir))
                    dir)
              installed-package-alist)))
    (setq duplicates
          (mapcar (lambda (dup) (sort (seq-filter
                                       (lambda (e) (equal dup (car e)))
                                       installed-package-alist)
                                      (lambda (a b)
                                        (string-greaterp (cdr a) (cdr b)))))
                  (find-duplicates (mapcar #'car installed-package-alist))))
    ;;     (dolist (dup duplicates))))
    ;; \      (pp (concat "rm -rf " (string-join old-files " "))))))
    ;; TODO: Delete all but newest duplicate.

    (pp (car duplicates))))

