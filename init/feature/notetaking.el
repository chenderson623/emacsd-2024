;;; -*- lexical-binding: t; -*-

(use-package denote
  :straight (denote :type git :host github :repo "protesilaos/denote" :branch "main")
  :commands (denote denote-open-or-create denote-link)
  :bind
  (("C-c n n" . denote)
   ("C-c n f" . denote-open-or-create)
   ("C-c n i" . denote-link)))

(provide 'feature/notetaking)

