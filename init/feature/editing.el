;;; -*- lexical-binding: t; -*-

;; https://github.com/akicho8/string-inflection
(use-package string-inflection
  :straight t
  :bind ("C-c C-u" . string-inflection-all-cycle))

(use-package expand-region
  :straight t
  :bind (
         ("C-=" . 'er/expand-region)
         ("C-+" . 'er/contract-region)))

(use-package ui/hydra/editing-mark
  ;; internal init
  :straight nil
  :bind ("C-c s" . hydra-mark/body))

(use-package multiple-cursors
  :straight t
  :config
  (setq mc/always-run-for-all nil))

(use-package ui/hydra/multiple-cursors
  ;; internal init
  :straight nil
  :commands multiple-cursors-hydra/body
  :bind ("C-c m" . editing>multiple-cursors-hydra)
  :init
  (defun editing>multiple-cursors-hydra ()
    (interactive)
    (require 'multiple-cursors)
    (multiple-cursors-hydra/body)))

(use-package anzu
  :straight t
  :bind (("M-%"   . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

(use-package zop-to-char
  :straight t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char))
  :config
  (progn
    (setq zop-to-char-kill-keys          '(?\C-w ?\C-k))
    (setq zop-to-char-copy-keys          '(?\M-w nil))
    (setq zop-to-char-next-keys          '(?\C-n nil))
    (setq zop-to-char-prec-keys          '(?\C-p nil))
    (setq zop-to-char-quit-at-pos-keys   '(?\C-g ?\e)) ; quit to original pos
    (setq zop-to-char-quit-at-point-keys '(?\r ?\C-q)) ; quit to current pos
    (setq zop-to-char-erase-keys         '(?\d ?\C-d))
  ))

(provide 'feature/editing)
