;;; -*- lexical-binding: t; -*-

;; https://github.com/akicho8/string-inflection
(use-package string-inflection
  :straight t
  :bind ("C-c C-u" . string-inflection-all-cycle))

;; (use-package expand-region
;;   :straight t
;;   :bind (
;;          ("C-=" . 'er/expand-region)
;;          ("C-+" . 'er/contract-region)))

(use-package expreg
  :straight t
  :bind (("C-=" . expreg-expand)
         ("C-+" . expreg-contract))
  :config
  (add-hook 'text-mode-hook
          (lambda ()
            (add-to-list 'expreg-functions #'expreg--sentence)))

  (defun expreg--line ()
  "Return a list of regions containing surrounding sentences."
  (ignore-errors
    (let (beg end)
      (end-of-visual-line)
      (setq end (point))
      (beginning-of-visual-line)
      (setq beg (point))
      `((line . ,(cons beg end))))))

  (setq-default expreg-functions
                '(expreg--subword
                  expreg--word
                  expreg--sentence
                  expreg--line
                  expreg--list
                  expreg--string
                  expreg--treesit
                  expreg--comment
                  expreg--paragraph-defun))
  )

(use-package selected
  :straight t
  :commands selected-minor-mode
  :init
  (setq selected-org-mode-map (make-sparse-keymap))
  (selected-global-mode 1)
  :bind (:map selected-keymap
              ("=" . er/expand-region)
              ("i" . indent-region)
              ("l" . downcase-region)
              ("m" . apply-macro-to-region-lines)
              ("q" . selected-off)
              ("r" . reverse-region)
              ("s" . sort-lines)
              ("u" . upcase-region)
              ("w" . count-words-region)
              ("y" . yank)
              :map selected-org-mode-map
              ("t" . org-table-convert-region)))

(use-package beginend
  :straight t
  :demand t
  :config
  (beginend-global-mode))

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
    (setq zop-to-char-kill-keys          '(?\C-w nil))
    ;;(setq zop-to-char-copy-keys          '(?\M-w nil))
    ;;(setq zop-to-char-next-keys          '(?\C-n nil))
    ;;(setq zop-to-char-prec-keys          '(?\C-p nil))
    (setq zop-to-char-quit-at-pos-keys   '(?\C-g ?\e)) ; quit to original pos
    (setq zop-to-char-quit-at-point-keys '(?\r ?\C-q)) ; quit to current pos
    (setq zop-to-char-erase-keys         '(?\d ?\C-k))
  ))

(provide 'feature/editing)
