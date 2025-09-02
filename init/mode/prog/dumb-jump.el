;;; dumb-jump.el --- description -*- lexical-binding: t; -*-

;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :straight t
  :commands (dumb-jump-xref-activate)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-rg-cmd "rg")
  (setq dumb-jump-max-find-time 5)
  (setq dumb-jump-force-searcher 'rg)
  )

(defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))

(provide 'mode/prog/dumb-jump)
;;; dumb-jump.el ends here
