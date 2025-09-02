;;; -*- lexical-binding: t; -*-

(use-package cape
  :straight t
  :bind ("C-c f" . cape-file))

;; https://github.com/company-mode/company-mode
;;  - https://company-mode.github.io/
(use-package company
  :straight t
  :commands (global-company-mode company-mode)
  :hook (after-init . global-company-mode) 
  :config
  (setq-default company-idle-delay 0.2
                company-show-numbers t
                company-selection-wrap-around t
                company-tooltip-align-annotations)
  (setq-default company-backends
                '((company-files
                   company-yasnippet
                   company-keywords
                   company-capf)
                  (company-abbrev company-dabbrev)))
  (setq-default company-global-modes
                '(not eshell-mode comint-mode minibuffer-inactive-mode))
  :bind (:map company-active-map
              ("\C-n" . #'company-select-next)
              ("\C-p" . #'company-select-previous)
              ("TAB" . #'company-select-next)
              ("<backtab>" . #'company-select-previous)
              ("RET" . company-complete-selection)
              ("<escape>" . company-abort)
              ("C-s" . company-filter-candidates)
              ("C-d" . company-show-doc-buffer)
              ("M-." . company-show-location)
              ("M-n" . nil)
              ("M-p" . nil)
              )
  )

(provide 'feature/completion-at-point)

