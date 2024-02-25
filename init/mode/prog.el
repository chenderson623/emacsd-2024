;;; -*- lexical-binding: t; -*-

(use-package auto-highlight-symbol
  :straight t
  :hook (prog-mode . auto-highlight-symbol-mode))

;;; Use Org Mode links in other modes
;; https://github.com/tarsius/orglink
(use-package orglink
  :straight t
  :hook (prog-mode . orglink-mode)
  :config
  (message "config orglink")
  ;;(global-orglink-mode)
  ;; Only enable this in Emacs Lisp mode, for now.
  ;;(setq orglink-activate-in-modes '(emacs-lisp-mode))
  )

;;; Highlight numbers in source code
;; https://github.com/Fanael/highlight-numbers
(use-package highlight-numbers
  :straight t
  :commands highlight-numbers-mode
  :hook (prog-mode . highlight-numbers-mode))

;;;;; Outline Navigation
;; Navigate elisp files easily. Outline is a built-in library and we can easily
;; configure it to treat elisp comments as headings.
;; [[help:outline-mode]]
(use-package outline
  :straight nil
  :hook (prog-mode . outline-minor-mode)
  :bind (:map outline-minor-mode-map
         ("<tab>"   . outline-cycle)
         ("S-<tab>" . outline-cycle-buffer)
         ("M-j"     . outline-move-subtree-down)
         ("M-k"     . outline-move-subtree-up)
         ("M-h"     . outline-promote)
         ("M-l"     . outline-demote)))

(use-package hl-todo
  :straight t
  :commands hl-todo-mode
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'markdown-mode-hook #'hl-todo-mode))

(provide 'mode/prog)

