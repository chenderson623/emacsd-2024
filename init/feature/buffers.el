;;; -*- lexical-binding: t; -*-

(use-package ibuffer
  :straight nil
  :commands (ibuffer ibuffer-jump)
  :hook
  ((ibuffer-mode . hl-line-mode)
))

(provide 'feature/buffers)

