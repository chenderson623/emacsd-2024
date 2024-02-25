;;; -*- lexical-binding: t; -*-

(defvar ispell-program-name (or (executable-find "ispell")
                               (executable-find "aspell")
                               (executable-find "hunspell")))

(when ispell-program-name
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))

(setq flyspell-use-meta-tab nil)

(provide 'feature/spellcheck)

