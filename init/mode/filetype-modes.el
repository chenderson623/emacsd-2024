;;; -*- lexical-binding: t; -*-

;;;; emacs-lisp
(use-package mode/language/elisp
  :straight nil
  ;; load at startup
  :demand t)

(use-package ini-mode :straight t :mode "\\.ini\\'")

;;;; json
;; https://github.com/json-emacs/json-mode
(use-package json-mode
  :straight t
  :mode "\\.json\\'"
  :config
  ;; https://github.com/DamienCassou/json-navigator
  (use-package json-navigator
    :straight t
    :commands json-navigator-navigate-region)
  ;; https://github.com/gongo/json-reformat
  (use-package json-reformat
    :straight t
    :commands json-reformat-region)
  ;; https://github.com/prettier/prettier-emacs
  (use-package prettier-js
    :straight t
    :hook
    (json-mode . prettier-js-mode)))

;;;; vimrc
(use-package vimrc-mode
  :straight t
  :mode "\\.vim\\(rc\\)?\\'"
  :mode "\\vifmrc\\'")

;;;; yaml
;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode :straight t :mode "\\.yml\\'" :mode "\\.yaml\\'")

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

(use-package mode/language/php
  :straight nil
  :mode ("\\.php\\'" . php-mode))

(provide 'mode/filetype-modes)

