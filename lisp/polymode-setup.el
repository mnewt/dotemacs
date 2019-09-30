;;; polymode-setup.el --- Set up Polymode -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(use-package polymode
  :config
  (with-no-warnings
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
    (define-innermode poly-restclient-elisp-single-innermode
      poly-restclient-elisp-root-innermode
      :head-matcher "^:[^ ]+ :="
      :tail-matcher "\n")
    (define-innermode poly-restclient-elisp-multi-innermode
      poly-restclient-elisp-root-innermode
      :head-matcher "^:[^ ]+ := <<"
      :tail-matcher "^#$")
    (define-polymode poly-restclient-mode
      :hostmode 'poly-restclient-hostmode
      :innermodes '(poly-restclient-elisp-single-innermode
                    poly-restclient-elisp-multi-innermode))

    ;; applescript
    (defun match-string-delimiter (ahead)
      "Match the delimiter of a string, forward if AHEAD is positive.
Backward if AHEAD is negative."
      (let ((re "[^\\]\""))
        (when (or (looking-at re)
                  (if (> ahead 0)
                      (re-search-forward re)
                    (re-search-backward re)))
          (cons (match-beginning 0) (match-end 0)))))

    (define-innermode poly-emacs-lisp-apples-innermode
      :mode 'apples-mode
      :head-matcher "do-applescript\s-*.*\""
      :tail-matcher #'match-string-delimiter)
    (define-polymode poly-emacs-lisp-mode
      :hostmode 'poly-emacs-lisp-hostmode
      :innermodes '(poly-emacs-lisp-apples-innermode)))

  ;;     ;; WIP
  ;;     ;; (define-innermode poly-emacs-lisp-Info-innermode
  ;;     ;;   :mode 'Info-mode
  ;;     ;;   :)

  :hook
  (js-mode-hook . poly-js-mode)
  ;; (rjsx-mode-hook . poly-rjsx-mode)
  (web-mode-hook . poly-web-mode)
  (restclient-mode-hook . poly-restclient-mode))

(use-package poly-markdown
  :hook
  (markdown-mode-hook . poly-markdown-mode))


(provide 'polymode-setup)

;;; polymode-setup.el ends here
