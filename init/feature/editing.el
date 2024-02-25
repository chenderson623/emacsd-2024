;;; -*- lexical-binding: t; -*-

;; https://github.com/akicho8/string-inflection
(use-package string-inflection
  :straight t
  :bind ("C-c C-u" . string-inflection-all-cycle))

(use-package expand-region
  :straight t
  :bind (
         ("C-=" . 'er/expand-region)
         ("C-+" . 'er/contract-region)
         ))

(use-package ui/hydra/editing-mark
  :bind ("C-c s" . hydra-mark/body)
)

(use-package multiple-cursors
  :straight t
  :config
  (setq mc/always-run-for-all nil)
)

(use-package ui/hydra/multiple-cursors
  :straight nil
  :commands multiple-cursors-hydra/body
  :bind ("C-c m" . editing>multiple-cursors-hydra)
  :init
  (defun editing>multiple-cursors-hydra ()
    (interactive)
    (require 'multiple-cursors)
    (multiple-cursors-hydra/body)))

(provide 'feature/editing)

