;;; -*- lexical-binding: t; -*-

(use-package display-line-numbers
  :straight (:type built-in)
  :hook prog-mode)

(use-package auto-highlight-symbol
  :straight t  
  :hook (prog-mode . auto-highlight-symbol-mode)
  ;;
  ;;; Key bindings:
  ;;
  ;; (defvar auto-highlight-symbol-mode-map
  ;;   (let ((map (make-sparse-keymap)))
  ;;     (define-key map (kbd "M-<left>") #'ahs-backward)
  ;;     (define-key map (kbd "M-<right>") #'ahs-forward)
  ;;     (define-key map (kbd "M-S-<left>") #'ahs-backward-definition)
  ;;     (define-key map (kbd "M-S-<right>") #'ahs-forward-definition)
  ;;     (define-key map (kbd "M--") #'ahs-back-to-start)
  ;;     (define-key map (kbd "C-x C-'") #'ahs-change-range)
  ;;     (define-key map (kbd "C-x C-a") #'ahs-edit-mode)
  ;;     map)
  ;;   "Keymap used in function `auto-highlight-symbol-mode'.")  
  )

;;; Use Org Mode links in other modes
;; https://github.com/tarsius/orglink
(use-package orglink
  :straight t
  :hook (prog-mode . orglink-mode))

;;; Highlight numbers in source code
;; https://github.com/Fanael/highlight-numbers
(use-package highlight-numbers
  :straight t
  :commands highlight-numbers-mode
  :hook (prog-mode . highlight-numbers-mode))

;;;;; Outline Navigation
;; Navigate elisp files easily. Outline is a built-in library and we can easily
;; configure it to treat elisp comments as headings.
;; [[help:outline-mode]]
(use-package outline
  :straight (:type built-in)  
  :bind (:map outline-minor-mode-map
              ("C-TAB" . outline-cycle)
	          ("<backtab>" . outline-cycle-buffer)
              ("C-<tab>" . outline-cycle)
              ("C-c C-n" . 'outline-next-visible-heading)
              ("C-c C-p" . 'outline-previous-visible-heading))
  :hook (prog-mode . outline-minor-mode))

;; Make outline faces look better
;; https://github.com/tarsius/outline-minor-faces
(use-package outline-minor-faces
  :straight t
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    'outline-minor-faces-add-font-lock-keywords))

(use-package hl-todo
  :straight t
  :commands hl-todo-mode
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'markdown-mode-hook #'hl-todo-mode))

(use-package rainbow-delimiters
  :straight t
  :hook prog-mode
  :commands rainbow-delimiters-mode)

(use-package smartparens
  :straight t
  :hook (prog-mode text-mode markdown-mode)
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package whitespace-cleanup-mode
  :straight t
  :hook prog-mode)

(provide 'mode/prog)

