;;; smart-jump.el --- description -*- lexical-binding: t; -*-

;; https://github.com/jojojames/smart-jump
(use-package smart-jump
  :straight t
  :after xref
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back)
         ("M-?" . smart-jump-references))
  :config
  ;; (smart-jump-setup-default-registers)
  (smart-jump-register :modes 'php-mode
                       :jump-fn 'xref-find-definitions
                       :pop-fn 'xref-pop-marker-stack
                       :refs-fn 'xref-find-references
                       :should-jump t
                       :heuristic 'point
                       :order 1)
  (setq smart-jump-find-references-fallback-function 'smart-jump-find-references-with-rg)

  )

(provide 'mode/prog/smart-jump)
;;; smart-jump.el ends here
