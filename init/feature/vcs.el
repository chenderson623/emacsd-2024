;;; -*- lexical-binding: t; -*-

(use-package magit
  :straight t
  :commands (magit magit-status)
)

;; (use-package git-gutter-fringe
;;   :straight t
;;   :defer t
;;   :init
;;   (global-git-gutter-mode t))

(use-package diff-hl
  :straight t
  :hook
  (prog-mode . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  )

(provide 'feature/vcs)
