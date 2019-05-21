;;; m-modes.el --- File Modes -*- lexical-binding: t -*-

;;; Commentary:

;; All modes which don't fit into a larger category

;;; Code:

;;;; WWW

(use-package shr-tag-pre-highlight
  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight))
  :commands
  (shr-tag-pre-highlight))

(use-package w3m
  :commands
  w3m)

;; Automate communication with services, such as nicserv
(with-eval-after-load 'erc
  (require 'erc-services)
  (erc-services-mode 1))

;;;; Log Files

(use-package vlf
  :custom
  (vlf-application 'dont-ask)
  :config
  (require 'vlf-setup))

(use-package logview
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
  :mode "Dockerfile\\'")

(use-package docker
  :bind
  ("C-c M-d" . docker))

(use-package docker-tramp
  :defer 2)

;; dw (https://gitlab.com/mnewt/dw)
(add-to-list 'auto-mode-alist '("\\DWfile.*\\'" . sh-mode))

;;;; Web

(use-package markdown-mode
  :mode "\\.md\\|markdown\\'"
  :custom
  (markdown-list-indent-width tab-width)
  (markdown-command "multimarkdown"))

(use-package web-mode
  :mode "\\.html\?\\'"
  :init
  ;; from web-mode FAQ to work with smartparens
  (defun m-web-mode-hook ()
    (setq web-mode-enable-auto-pairing nil))
  (defun sp-web-mode-is-code-context (_id action _context)
    (and (eq action 'insert)
         (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'")
  :custom
  (sgml-basic-offset tab-width)
  (web-mode-markup-indent-offset tab-width)
  (web-mode-css-indent-offset tab-width)
  (web-mode-code-indent-offset tab-width)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-ac-sources-alist '(("css" . (ac-source-css-property))
                               ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  :hook
  (web-mode . m-web-mode-hook))

(use-package company-web
  :commands
  (company-web-html)
  :hook
  (web-mode . (lambda () (set (make-local-variable 'company-backends)
                              (cons 'company-web-html company-backends)))))

(use-package sass-mode
  :mode "\\(?:s\\(?:[ac]?ss\\)\\)")

(use-package know-your-http-well
  :commands
  (http-header http-method http-relation http-status-code))

(use-package restclient
  :mode "\\.restclient\\'"
  :commands
  (restclient-mode restclient-outline-mode))

(use-package company-restclient
  :hook
  (restclient-mode . (lambda () (add-to-list 'company-backends 'company-restclient))))

;;;; Javascript

(use-package add-node-modules-path
  :hook
  ((css-mode graphql-mode js2-mode markdown-mode web-mode) . add-node-modules-path))

(use-package js2-mode
  :mode "\\.js\\'"
  :custom
  ;; Set tab width for js-mode and json-mode
  (js-indent-level tab-width)
  (js2-basic-offset tab-width)
  :hook
  (js2-mode . js2-imenu-extras-mode))

(use-package rjsx-mode
  :mode "\\.js[mx]\\'")

;; Tide is for Typescript but it works great for js/react.
(use-package tide
  :custom
  (tide-format-options `(:indentSize ,tab-width :tabSize ,tab-width))
  (tide-default-mode "JS")
  :commands
  (tide-setup tide-hl-identifier-mode)
  :hook
  ((js2-mode typescript-mode) . (lambda ()
                                  (tide-setup)
                                  ;; Let tide do the symbol highlighting.
                                  (symbol-overlay-mode -1)
                                  (tide-hl-identifier-mode)
                                  ;; Because we use prettier instead.
                                  (setq-local flycheck-checkers
                                              (remove 'jsx-tide flycheck-checkers)))))

(use-package prettier-js
  ;; :ensure-system-package
  ;; (prettier . "npm i -g prettier")
  :hook
  ((graphql-mode js-mode js2-mode json-mode sass-mode web-mode)  . prettier-js-mode))

(use-package indium
  ;; :ensure-system-package
  ;; (indium . "npm i -g indium")
  :custom
  (indium-chrome-executable "/Applications/Chromium.app/Contents/MacOS/Chromium")
  (indium-chrome-use-temporary-profile nil)
  :commands
  (indium-connect indium-launch))

(use-package json-mode
  ;; :ensure-system-package jq
  :mode "\\.json\\|prettierrc\\'")

(use-package graphql-mode
  :mode "\\(?:\\.g\\(?:\\(?:raph\\)?ql\\)\\)\\'")

;;;; Python

(use-package elpy
  ;; :ensure-system-package
  ;; jedi doesn't have an executable and there's not one single path we can look
  ;; across OS and python versions, so just assume it comes with flake8.
  ;; ((flake8 . "pip install jedi flake8")
  ;;  (black . "pip install black")
  ;;  (yapf . "pip install yapf"))
  :interpreter ("python3?" . python-mode)
  :custom
  (gud-pdb-command-name "python -m pdb")
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  :commands
  (elpy-black-fix-code)
  :hook
  (python-mode . (lambda ()
                   (unless (bound-and-true-p elpy-version) (elpy-enable))
                   (add-hook 'before-save-hook #'elpy-black-fix-code nil t)))
  (python-mode . flycheck-mode)
  :bind
  (:map python-mode-map
        ("s-<return>" . elpy-shell-send-statement)))

(use-package company-jedi
  :hook
  (python-mode . (lambda () (set (make-local-variable 'company-backends) '(company-jedi)))))

;;;; Other Modes

;; display nfo files in all their glory
;; https://github.com/wasamasa/dotemacs/blob/master/init.org#display-nfo-files-with-appropriate-code-page)
(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))

;; perl
(setq perl-indent-level tab-width)

;; systemd
(add-to-list 'auto-mode-alist '("\\(?:\\.service\\|\\.timer\\)\\'" . conf-mode))

;; DNS
(add-to-list 'auto-mode-alist '("\\.rpz\\'" . dns-mode))

(setq-default css-indent-offset tab-width)

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
  (nginx-indent-level tab-width))

(use-package caddyfile-mode
  :mode "\\`Caddyfile\\'")

(use-package yaml-mode
  :mode "\\.ya\?ml\\'")

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package enh-ruby-mode
  ;; :ensure-system-package
  ;; (rufo . "gem install rufo")
  :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'")

(use-package inf-ruby
  :hook
  (enh-ruby-mode . inf-ruby-minor-mode)
  (compilation-filter . inf-ruby-auto-enter)
  :commands
  (inf-ruby inf-ruby-console-auto)
  :bind
  (:map inf-ruby-minor-mode-map
        ("s-<return>". ruby-send-last-sexp)
        ("C-M-x" . ruby-send-block)))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package go-mode
  :mode "\\.go\\'")

(use-package company-go
  :hook
  (go-mode . (lambda () (set (make-local-variable 'company-backends) '(company-go)))))

(use-package powershell
  :mode "\\.ps1\\'"
  :custom
  (powershell-indent tab-width)
  (powershell-continuation-indent tab-width))

(use-package php-mode
  :mode "\\.php\\'")

(use-package ios-config-mode
  :mode "\\.cfg\\'")

;;;; Utility

(use-package polymode
  :config
  
;;;;; rjsx
  (define-hostmode poly-rjsx-hostmode
    :mode 'rjsx-mode)
  (define-innermode poly-rjsx-graphql-innermode
    :mode 'graphql-mode
    :head-matcher "graphql[ \t\n]*(?`"
    :tail-matcher "`"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-rjsx-mode
    :hostmode 'poly-rjsx-hostmode
    :innermodes '(poly-rjsx-graphql-innermode))
  
;;;;; web
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
  
;;;;; restclient
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
  ((rjsx-mode . poly-rjsx-mode)
   (web-mode . poly-web-mode)
   (restclient-mode . poly-restclient-mode)))

(use-package poly-markdown)

(use-package fence-edit
  :straight
  (:type git :host github :repo "mnewt/fence-edit.el")
  :config
  (add-multiple-to-list 'fence-edit-blocks
                        '(("---" "---" yaml)
                          ("+++" "+++" toml)
                          ("graphql[ \t\n]*(?`" "`" graphql)
                          ("<svg" "</svg>" nxml t)
                          ("<html" "</html>" web t)
                          ("<div" "</div>" web t)))
  :hook
  ;; Don't shadow the fence-edit binding
  (markdown-mode . (lambda () (bind-key "C-c '" nil markdown-mode-map)))
  :bind
  ("C-c '" . fence-edit-dwim))

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

(provide 'm-modes)

;;; m-modes.el ends here
