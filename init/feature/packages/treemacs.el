;;; treemacs.el --- description -*- lexical-binding: t; -*-

;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :straight t
  :bind (("C-c t t" . +treemacs-toggle)
         ("C-c p t" . treemacs-add-and-display-current-project))
  :init
  (setq treemacs-follow-after-init t
        treemacs-project-follow-mode t
        treemacs-is-never-other-window t
        treemacs-space-between-root-nodes nil
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-persist-file (emacs-cache*filepath "treemacs-persist")
        treemacs-last-error-persist-file (emacs-cache*filepath "treemacs-last-error-persist"))
  :config
  (treemacs-follow-mode t)
  (defun +treemacs-toggle ()
    "Initialize or toggle treemacs."
    (interactive)
    (require 'treemacs)
    (pcase (treemacs-current-visibility)
      (`visible (delete-window (treemacs-get-local-window)))
      (_ (treemacs-add-and-display-current-project)))))

;; https://github.com/rainstormstudio/treemacs-nerd-icons
(use-package treemacs-nerd-icons
  :straight t
  :init (with-eval-after-load  'lsp-treemacs)
  (require 'treemacs-nerd-icons)
  :config
  (treemacs-load-theme "nerd-icons"))


(provide 'feature/packages/treemacs)
;;; treemacs.el ends here
