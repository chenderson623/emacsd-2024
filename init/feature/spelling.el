;;; -*- lexical-binding: t; -*-

;;(defvar ispell-program-name (or (executable-find "ispell")
;;                               (executable-find "aspell")
;;                               (executable-find "hunspell")))

;;(when ispell-program-name
;;  (add-hook 'text-mode-hook #'flyspell-mode)
;;  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

;;(setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))

;;(setq flyspell-use-meta-tab nil)

(use-package ispell
  :commands (ispell-word ispell-region ispell-buffer)
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")
    ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

(use-package flyspell
  :config
  (setq flyspell-abbrev-p t
        flyspell-use-global-abbrev-table-p t
        flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  :hook ((markdown-mode . flyspell-mode)
         (org-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;; completion of spellings
;; https://github.com/d12frosted/flyspell-correct
(use-package flyspell-correct
  :straight t
  :after flyspell
  :bind (:map flyspell-mode-map
         ("C-;" . flyspell-correct-previous)
         ("C-:" . flyspell-correct-at-point))
  :custom
  (flyspell-correct-interface #'flyspell-correct-completing-read))

;; use avy interface for flyspell
;; https://github.com/d12frosted/flyspell-correct
(use-package flyspell-correct-avy-menu
  :after flyspell-correct)

;;;;; Spelling Goto Next Error
(defun my-spelling>ispell-goto-next-error ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(provide 'feature/spellcheck)

