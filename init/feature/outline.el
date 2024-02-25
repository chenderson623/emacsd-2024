;;; outline.el --- setup outline -*- lexical-binding: t; -*-

(use-package outline
  :straight (:type built-in)  
  :bind (:map outline-minor-mode-map
              ("TAB" . outline-cycle)
	          ("<backtab>" . outline-cycle-buffer)
              ("<tab>" . outline-cycle)
              ("C-c C-n" . 'outline-next-visible-heading)
              ("C-c C-p" . 'outline-previous-visible-heading))
  :hook (prog-mode . outline-minor-mode))

;; Make outline faces look better
(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    'outline-minor-faces-add-font-lock-keywords))

(provide 'feature/outline)

