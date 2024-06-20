;;; ibuffer.el --- description -*- lexical-binding: t; -*-

(use-package ibuffer
  :straight nil
  :commands (ibuffer ibuffer-jump)
  :hook
  ((ibuffer-mode . hl-line-mode)
))

(provide 'mode/ibuffer)
;;; ibuffer.el ends here
