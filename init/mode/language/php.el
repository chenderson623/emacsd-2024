;;;-*- lexical-binding: t; -*-

;; https://github.com/xcwen/ac-php
(use-package ac-php
  :straight t
  :after php)

(defun php:mode-hook ()
  "Configuration for php."
  (setq indent-tabs-mode nil
        c-basic-offset 4
        php-template-compatibility nil
        php-mode-coding-style 'psr2)
  (subword-mode 1)
  (setq ac-sources '(ac-source-php))
  (ac-php-core-eldoc-setup))

;; https://github.com/emacs-php/php-mode
(use-package php-mode
  :straight t
  :init
  (add-hook 'php-mode-hook #'php:mode-hook)
  )

;; https://github.com/emacs-php/psysh.el
(use-package psysh 
  :straight t
  :commands psysh)

(provide 'mode/language/php)
