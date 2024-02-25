;;; -*- lexical-binding: t; -*-

;;;;; Server
;; start server for emacsclient
(use-package server
  :straight nil
  :if window-system
  :hook (after-init . server-mode))

(provide 'emacs/server)

