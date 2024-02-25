;;; -*- lexical-binding: t; -*-

;; https://codeberg.org/ideasman42/emacs-elisp-autofmt
(use-package elisp-autofmt
  :straight
  (elisp-autofmt :type git :host codeberg :repo "ideasman42/emacs-elisp-autofmt")
  :commands
  (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook
  (emacs-lisp-mode . elisp-autofmt-mode)
  :config
  ;;(setq elisp-autofmt-load-packages-local ("use-package"))
  (setq elisp-autofmt-python-bin "python3")
  (setq elisp-autofmt-style 'fixed))

(provide 'trying/elisp-autofmt)
