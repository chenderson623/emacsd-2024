;;; -*- lexical-binding: t; -*-

(require 'mode/prog/treesit)

;;;; emacs-lisp
(use-package mode/language/elisp
  :straight nil
  ;; load at startup
  :demand t)

(use-package ini-mode
  :straight t
  :mode "\\.ini\\'")

;;;; json
(use-package mode/language/json
  :straight nil
  :commands (json:json-mode-init)
  ;; :mode ("\\.json\\'" . json-ts-mode)
  :mode ("\\.json\\'" . json-mode)
  :hook (json-ts-mode . json:json-mode-init)
  :hook (json-mode . json:json-mode-init)
  :config
  (message "Loaded mode/language/json"))

;;;; php
(use-package mode/language/php
  :straight nil
  :commands (php:php-mode-init)
  ;;:mode ("\\.php\\'" . php-mode)
  :mode ("\\.php\\'" . php-ts-mode)
  :hook (php-ts-mode . php:php-mode-init)
  :hook (php-mode . php:php-mode-init)
  :config
  (message "Loaded mode/language/php"))

;;;; vimrc
(use-package vimrc-mode
  :straight t
  :mode "\\.vim\\(rc\\)?\\'"
  :mode "\\vifmrc\\'")

;;;; yaml
;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :straight t
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'")

;;;; Dockerfile
(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile\\'")

(use-package lua-mode
  :straight (:host github :repo "immerrr/lua-mode")
  :mode ("\\.lua\\'")
  :interpreter "lua")

(use-package git-modes
  :straight t
  :mode (("\\.gitattributes\\'" . gitattributes-mode)
         ("\\.gitconfig\\'" . gitconfig-mode)
         ("\\.gitignore\\'" . gitignore-mode)))

(provide 'mode/filetype-modes)

