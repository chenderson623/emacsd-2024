;;; -*- lexical-binding: t; -*-

;;;; Affe
;; https://github.com/minad/affe
;; This package provides an asynchronous fuzzy finder similar to the fzf command-line fuzzy finder, written in pure Elisp.
(use-package affe
  :straight t
  :after orderless
  :commands (affe-find affe-grep)
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key
                     (kbd "M-."))
  ;; The default regular expression transformation of Consult is limited. It is recommended to configure Orderless as affe-regexp-compiler in Consult.
  (defun affe-orderless-regexp-compiler
      (input _type _ignorecase)
    (setq input
          (orderless-pattern-compiler input))
    (cons input
          (lambda
            (str)
            (orderless--highlight input str))))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

(provide 'feature/files)
