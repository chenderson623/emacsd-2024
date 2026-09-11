;;; -*- lexical-binding: t; -*-

;;;;; Server
;; start server for emacsclient
(use-package server
  :straight nil
  :if window-system
  :init
  (setq server-auth-dir (emacs-state*filepath "server"))
  :hook (after-init . server-mode))

(provide 'init-server)

