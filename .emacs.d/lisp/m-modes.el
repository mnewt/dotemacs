;;; m-modes.el --- File Modes -*- lexical-binding: t -*-

;;; Commentary:

;; All modes which don't fit into a larger category

;;; Code:

(use-package lsp-mode
  :custom
  (lsp-auto-guess-project-root t)
  (lsp-eldoc-render-all t)
  (lsp-prefer-flymake nil)
  :commands
  (lsp lsp-deferred)
  :hook
  ((c-mode-hook c++-mode-hook css-mode-hook go-mode-hook
                java-mode-hook js-mode-hook php-mode-hook
                powershell-mode-hook elpy-mode-hook
                enh-ruby-mode-hook nxml-mode-hook rust-mode-hook
                sass-mode-hook sh-mode-hook html-mode-hook
                web-mode-hook xml-mode-hook) . lsp-deferred)
  (lsp-after-open-hook . lsp-enable-imenu))

(use-package lsp-ui :commands lsp-ui-mode
  :bind
  (:map lsp-ui-mode-map
        ("M-." . lsp-ui-peek-find-definitions)
        ("M-?" . lsp-ui-peek-find-references)
        ("C-h ." . lsp-ui-doc-mode)))

(use-package company-lsp :commands company-lsp)

;; (use-package dap-mode)

(use-package format-all
  :commands
  (format-all-buffer format-all-region)
  :hook
  ((css-mode-hook dockerfile-mode-hook enh-ruby-mode-hook
                  go-mode-hook lua-mode-hook php-mode-hook
                  python-mode-hook ruby-mode-hook toml-mode-hook
                  web-mode-hook yaml-mode-hook) .
                  format-all-mode))

(use-package sh-script
  :mode ("\\.sh\\'" . sh-mode)
  :interpreter ("sh" . sh-mode) ("bash" . sh-mode)
  :init
  (defun maybe-reset-major-mode ()
    "Reset the buffer's `major-mode' if a different mode seems like a better fit.
Mostly useful as a `before-save-hook', to guess mode when saving
a new file for the first time."
    (when (and (buffer-file-name)
               (not (file-exists-p (buffer-file-name)))
               (eq major-mode 'fundamental-mode))
      (normal-mode)))

  :custom
  (sh-basic-offset tab-width)
  (sh-indentation tab-width)
  ;; Tell `executable-set-magic' to insert #!/usr/bin/env interpreter
  (executable-prefix-env t)
  :config
  ;; Match variables in quotes. Fuco1 is awesome, mkay.
  ;; https://fuco1.github.io/2017-06-11-Font-locking-with-custom-matchers.html
  (defun shell-match-variables-in-quotes (limit)
    "Match variables in double-quotes in `sh-mode' with LIMIT."
    (with-syntax-table sh-mode-syntax-table
      (catch 'done
        (while (re-search-forward
                ;; `rx' is cool, mkay.
                (rx (or line-start (not (any "\\")))
                    (group "$")
                    (group
                     (or (and "{" (+? nonl) "}")
                         (and (+ (any alnum "_")))
                         (and (any "*" "@" "#" "?" "-" "$" "!" "0" "_")))))
                limit t)
          (-when-let (string-syntax (nth 3 (syntax-ppss)))
            (when (= string-syntax 34)
              (throw 'done (point))))))))

  (font-lock-add-keywords 'sh-mode '((shell-match-variables-in-quotes
                                      (1 'default t)
                                      (2 font-lock-variable-name-face t))))
  :hook
  (before-save-hook . maybe-reset-major-mode)
  (after-save-hook . executable-make-buffer-file-executable-if-script-p)
  :bind*
  (:map sh-mode-map
        ("<return>" . newline-and-indent)
        ("RET" . newline-and-indent)))

;;;; Log Files

(use-package vlf
  :defer 5
  :custom
  (vlf-application 'dont-ask)
  :config
  (require 'vlf-setup))

(use-package logview
  :mode "\\.log.*"
  :custom
  (logview-additional-timestamp-formats
   '(("ISO 8601 datetime (with 'T' and 'Z') + millis"
      (java-pattern . "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"))
     ("millis"
      (java-pattern . "SSSSS"))))
  (logview-additional-level-mappings
   '(("VMware" . ((error "error")
                  (warning "warning")
                  (information "info")
                  (debug "debug")
                  (trace "trace")))
     ("NPM" . ((error "error")
               (warning "warn")
               (information "info")
               (debug "verbose")
               (trace "silly")))))
  (logview-additional-submodes
   '(("VMware" . ((format  . "TIMESTAMP LEVEL NAME [THREAD] ")
                  (levels  . "VMware")))
     ("NPM" . ((format . "TIMESTAMP LEVEL NAME THREAD")
               (levels . "NPM"))))))

;;;; Docker

(use-package dockerfile-mode
  :mode "\\`Dockerfile")

(use-package docker
  :bind
  ("C-c M-d" . docker))

(use-package docker-tramp
  :after tramp
  :defer 5)

;; dw (https://gitlab.com/mnewt/dw)
(add-to-list 'auto-mode-alist '("\\`DWfile" . sh-mode))

;;;; Web

(use-package eww
  :config
  (use-package shr-tag-pre-highlight
    :after shr
    :hook
    (eww-mode-hook . (lambda () (add-to-list 'shr-external-rendering-functions
                                             '(pre . shr-tag-pre-highlight))))
    :commands
    (shr-tag-pre-highlight))
  :commands
  eww)

(use-package w3m
  :custom
  (w3m-search-engine-alist
   '(("google" "https://www.google.com/search?q=%s&ie=utf-8&oe=utf-8&gbv=1" utf-8)
     ("emacswiki" "https://www.emacswiki.org/cgi-bin/wiki?search=%s")
     ("en.wikipedia" "https://en.wikipedia.org/wiki/Special:Search?search=%s")
     ("duckduckgo" "https://duckduckgo.com/lite&q=%s" utf-8)))
  (w3m-search-default-engine "duckduckgo")
  :commands
  (w3m w3m-goto-url w3m-search))

(use-package markdown-mode
  :mode "\\.md\\|markdown\\'"
  :custom
  (markdown-list-indent-width tab-width)
  (markdown-command "multimarkdown"))

(use-package web-mode
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'")
  :init
  ;; from web-mode FAQ to work with smartparens
  (defun web-mode-setup ()
    (setq web-mode-enable-auto-pairing nil))

  (defun sp-web-mode-is-code-context (_id action _context)
    (and (eq action 'insert)
         (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))
  :custom
  (sgml-basic-offset tab-width)
  (web-mode-markup-indent-offset tab-width)
  (web-mode-css-indent-offset tab-width)
  (web-mode-code-indent-offset tab-width)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-ac-sources-alist
   '(("css" . (ac-source-css-property))
     ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  :config
  (use-package company-web
    :commands
    (company-web-html)
    :hook
    (web-mode-hook . (lambda () (set (make-local-variable 'company-backends)
                                     (cons 'company-web-html company-backends)))))
  :hook
  (web-mode-hook . web-mode-setup))

(use-package css-mode
  :mode "\\.css\\'"
  :custom
  (css-indent-offset tab-width))

(use-package sass-mode
  :ensure-system-package
  (sass . "gem install sass")
  :mode "\\(?:s\\(?:[ac]?ss\\)\\)")

(use-package restclient
  :mode "\\.restclient\\'"
  :config
  (use-package company-restclient
    :hook
    (restclient-mode-hook
     . (lambda () (add-to-list 'company-backends 'company-restclient))))

  (use-package know-your-http-well
    :commands
    (http-header http-method http-relation http-status-code))

  :commands
  (restclient-mode restclient-outline-mode))

;;;; Javascript

(use-package add-node-modules-path
  :hook
  ((css-mode-hook
    graphql-mode-hook
    js2-mode-hook
    markdown-mode-hook
    sass-mode-hook
    web-mode-hook) . add-node-modules-path))

(use-package js
  :mode ("\\.jsx?\\'" . js-mode)
  :custom
  (js-indent-level tab-width))

(use-package json-mode
  :ensure-system-package jq
  :mode "\\.json\\|prettierrc\\'")

(use-package graphql-mode
  :mode "\\(?:\\.g\\(?:\\(?:raph\\)?ql\\)\\)\\'")

;;;; Python

(use-package python
  :mode "\\.py\\'"
  :ensure-system-package
  ;; jedi doesn't have an executable and there's not one single path we can look
  ;; across OS and python versions, so just let it tag along.
  ((black . "pip install black")
   (python-language-server . "pip install python-language-server"))
  :interpreter "python3?"
  :custom
  (gud-pdb-command-name "python -m pdb")
  :bind
  (:map python-mode-map
        ("s-<return>" . python-shell-send-defun)))

;;;; Other Modes

;; git config files
(add-to-list 'auto-mode-alist '("\\.git\\(?:config\\|ignore\\).*" . conf-mode))
;; SSH server config files
(add-to-list 'auto-mode-alist '("sshd\?_config" . conf-mode))

;; display nfo files in all their glory
;; https://github.com/wasamasa/dotemacs/blob/master/init.org#display-nfo-files-with-appropriate-code-page)
(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))

(use-package perl-mode
  :mode "\\.pl\\'"
  :custom
  (perl-indent-level tab-width))

(use-package fish-mode
  :mode "\\.fish\\'"
  :custom (fish-indent-offset tab-width))

(use-package systemd
  :mode
  ("\\.\\(?:automount\\|link\\|mount\\|net\\(?:dev\\|work\\)\\|path\\|s\\(?:ervice\\|lice\\|ocket\\)\\|t\\(?:arget\\|imer\\)\\)\\'" . systemd-mode))

;; DNS
(use-package dns-mode
  :mode "\\.rpz\\'")

;; (use-package genrnc
;;   :custom
;;   (genrnc-user-schemas-directory "~/.emacs.d/schema")
;;   :commands
;;   (genrnc-regist-file))

;; (use-package rnc-mode
;;   :mode "\\.rnc\\'")

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package nginx-mode
  :custom
  (nginx-indent-level tab-width)
  :commands
  (nginx-mode))

(use-package caddyfile-mode
  :mode "\\`Caddyfile.*")

(use-package yaml-mode
  :mode "\\.ya\?ml\\'")

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package enh-ruby-mode
  :ensure-system-package
  (rufo . "gem install rufo")
  :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
  :config
  (use-package inf-ruby
    :hook
    (enh-ruby-mode-hook . inf-ruby-minor-mode)
    (compilation-filter . inf-ruby-auto-enter)
    :commands
    (inf-ruby inf-ruby-console-auto)
    :bind
    (:map inf-ruby-minor-mode-map
          ("s-<return>". ruby-send-last-sexp)
          ("C-M-x" . ruby-send-block))))

(use-package lua-mode
  :mode "\\.lua\\'"
  :custom
  (lua-indent-level tab-width))

(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-indent-offset tab-width))

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (use-package company-go
    :hook
    (go-mode-hook
     . (lambda () (set (make-local-variable 'company-backends) '(company-go))))))

(use-package powershell
  :mode "\\.ps1\\'"
  :custom
  (powershell-indent tab-width)
  (powershell-continuation-indent tab-width))

(use-package php-mode
  :mode "\\.php\\'")

(use-package IOS-config-mode
  :git "https://github.com/nibrahim/IOS-config-mode.git"
  :mode "\\.cfg\\'")

;;;; Utility

(use-package polymode
  :commands
  pm--get-keylist.keymap-from-parent
  pm--config-name
  :config
  ;; rjsx
  ;; (define-hostmode poly-rjsx-hostmode
  ;;   :mode 'rjsx-mode)
  ;; (define-innermode poly-rjsx-graphql-innermode
  ;;   :mode 'graphql-mode
  ;;   :head-matcher "graphql[ \t\n]*(?`"
  ;;   :tail-matcher "`"
  ;;   :head-mode 'host
  ;;   :tail-mode 'host)
  ;; (define-polymode poly-rjsx-mode
  ;;   :hostmode 'poly-rjsx-hostmode
  ;;   :innermodes '(poly-rjsx-graphql-innermode))

  ;; js
  (define-hostmode poly-js-hostmode
    :mode 'js-mode)
  (define-innermode poly-js-graphql-innermode
    :mode 'graphql-mode
    :head-matcher "graphql[ \t\n]*(?`"
    :tail-matcher "`"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-js-mode
    :hostmode 'poly-js-hostmode
    :innermodes '(poly-js-graphql-innermode))
  
  ;; web
  (define-hostmode poly-web-hostmode
    :mode 'web-mode)
  (define-innermode poly-web-svg-innermode
    :mode 'nxml-mode
    :head-matcher "<svg"
    :tail-matcher "</svg>"
    :head-mode 'inner
    :tail-mode 'inner)
  (define-polymode poly-web-mode
    :hostmode 'poly-web-hostmode
    :innermodes '(poly-web-svg-innermode))
  
  ;; restclient
  (define-hostmode poly-restclient-hostmode
    :mode 'restclient-mode)
  (define-innermode poly-restclient-elisp-root-innermode
    :mode 'emacs-lisp-mode
    :head-mode 'host
    :tail-mode 'host)
  (define-innermode poly-restclient-elisp-single-innermode poly-restclient-elisp-root-innermode
    :head-matcher "^:[^ ]+ :="
    :tail-matcher "\n")
  (define-innermode poly-restclient-elisp-multi-innermode poly-restclient-elisp-root-innermode
    :head-matcher "^:[^ ]+ := <<"
    :tail-matcher "^#$")
  (define-polymode poly-restclient-mode
    :hostmode 'poly-restclient-hostmode
    :innermodes '(poly-restclient-elisp-single-innermode
                  poly-restclient-elisp-multi-innermode))

  :hook
  ((js-mode-hook . poly-js-mode)
   ;; (rjsx-mode-hook . poly-rjsx-mode)
   (web-mode-hook . poly-web-mode)
   (restclient-mode-hook . poly-restclient-mode)))

(use-package poly-markdown
  :hook
  (markdown-mode-hook . poly-markdown-mode))

(use-package fence-edit
  :git "https://github.com/aaronbieber/fence-edit.el.git"
  :config
  (seq-doseq (e '(("---" "---" yaml)
                  ("+++" "+++" toml)
                  ("graphql[ \t\n]*(?`" "`" graphql)
                  ("<svg" "</svg>" nxml t)
                  ("<html" "</html>" web t)
                  ("<div" "</div>" web t)))
    (add-to-list 'fence-edit-blocks e))
  :hook
  ;; Don't shadow the fence-edit binding
  (markdown-mode-hook . (lambda () (bind-key "C-c '" nil markdown-mode-map)))
  :bind
  ("C-c '" . fence-edit-dwim))

(provide 'm-modes)

;;; m-modes.el ends here
