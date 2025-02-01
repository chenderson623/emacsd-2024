;;; -*- lexical-binding: t; -*-

;;; dired-aux.el
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/dired-aux.el
(use-package dired-aux
  :straight nil
  :commands (dired-do-async-shell-command dired-isearch-filenames)
  :bind
  (:map
   dired-mode-map
   ("M-s M-s" . dired-do-async-shell-command)
   ("C-s" . dired-isearch-filenames)))

;;;; dired-hide-dotfiles
;; https://github.com/mattiasb/dired-hide-dotfiles
(use-package dired-hide-dotfiles
  :straight t
  :after dired
  :bind (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

;;;; dired-subtree
;; Insert subdirectories in a tree-like fashion
;; https://github.com/Fuco1/dired-hacks
(use-package dired-subtree
  :straight t
  :after dired
  :bind (:map dired-mode-map ("TAB" . dired-subtree-toggle)))

;;;; dired-narrow
;; https://github.com/Fuco1/dired-hacks
(use-package dired-narrow
  :straight t
  :after dired
  :bind (:map dired-mode-map ("/" . dired-narrow)))

;;;; dired-collapse
;; https://github.com/Fuco1/dired-hacks
;; Flaten display of nested directories with no other content.
(use-package dired-collapse
  :straight t
  :after dired)

;; from https://protesilaos.com/emacs/dired-preview
;; call `dired-preview-mode`
(use-package dired-preview
  :straight (dired-preview
             :type git
             :host github
             :repo "protesilaos/dired-preview"
             :branch "main")
  :after dired)

(use-package disk-usage
  :straight t
  :after dired
  :bind
  (:map dired-mode-map ("C-c d S" . disk-usage-here)) ;; <---- TODO C-c d is a good dired mode map?
  :init
  (setq disk-usage--format-files 'file-name-nondirectory))

(provide 'mode/extended/dired)
