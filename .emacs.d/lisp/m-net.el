;;; m-net.el --- Network Utils -*- lexical-binding: t -*-

;;; Commentary:

;; These are some network utilities.

;;; Code:

;; Automate communication with services, such as nicserv.
(use-package erc
  :hook
  (erc-connect-pre . erc-services-mode))

(use-package url
  :config
  (defun public-ip ()
    "Display the local host's apparent public IP address."
    (interactive)
    (url-retrieve "https://diagnostic.opendns.com/myip"
                  (lambda (_)
                    (goto-char (point-min))
                    (re-search-forward "^$")
                    (delete-char 1)
                    (delete-region (point) (point-min))
                    (let ((ip (buffer-string)))
                      (kill-new ip)
                      (message ip)))))
  :commands
  (public-ip url-retrieve))

(defun dis (hostname)
  "Resolve a HOSTNAME to its IP address."
  (interactive "MHostname: ")
  (message (shell-command-to-string
            (concat "drill "
                    hostname
                    " | awk '/;; ANSWER SECTION:/{flag=1;next}/;;/{flag=0}flag'"))))

(defun ips ()
  "Show the machine's IP addresses."
  (interactive)
  (shell-command
   (pcase system-type
     ('gnu/linux
      "ip address show | awk '/inet /{if ($5 != \"lo\") { print $7 \": \" $2 }}'")
     ('darwin
      "/sbin/ifconfig | awk '/^[a-z0-9]+:/{ i=$1 } /inet / { if (i != \"lo0:\") { print i \" \" $2 }}'")
     ('cygwin
      "ipconfig | awk -F' .' '/Address/ {print $NF}'"))))

(use-package wttrin
  :custom
  (wttrin-default-cities '("Albany CA"
                           "San Francisco CA"
                           "Austin TX"
                           "Eugene OR"
                           "Truckee CA"
                           "Moon"))
  (wttrin-default-accept-language '("Accept-Language" . "en-US"))
  :config
  (defun advice-delete-other-windows (&rest _)
    "Advice that will delete other windows."
    (delete-other-windows))

  (advice-add 'wttrin :before #'advice-delete-other-windows)
  :bind
  ("C-c M-w" . wttrin))

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

(provide 'm-net)

;;; m-net.el ends here
