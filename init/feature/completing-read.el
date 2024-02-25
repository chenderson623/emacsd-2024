;;; -*- lexical-binding: t; -*-

;;;; Consult
;; Consult provides search and navigation commands based on the Emacs completion function
;; https://github.com/minad/consult
(use-package consult
  :straight t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (
	 ;; C-x bindings
	 ("C-x b" . consult-buffer) ;; switch-to-buffer
	 ;;("C-x C-b" . consult-buffer) ;; switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; switch-to-buffer-other-window
	 ("C-x r b" . consult-bookmark) ;; bookmark-jump
	 ("C-x p b" . consult-project-buffer) ;; project-switch-to-buffer

	 ;; M- bindings
	 ("M-y" . consult-yank-pop) ;; yank-pop

	 ;; M-g bindings (goto-map)
	 ("M-g g" . consult-goto-line) ;; goto-line
	 ("M-g M-g" . consult-goto-line) ;; goto-line
	 ("M-g o" . consult-outline)
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-marks)

	 ;; M-s bindings (search-map)
	 ("M-s d" . consult-find)
	 ("M-s D" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s m" . consult-multi-occur)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)

	 ;; Isearch bindings
	 ("M-s e" . consult-isearch-history)

	 :map
	 isearch-mode-map
	 ("M-e" . consult-isearch-history) ;; isearch-edit-string
	 ("M-s e" . consult-isearch-history) ;; isearch-edit-string
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)

	 ;; Minibuffer bindings
	 :map
	 minibuffer-local-map
	 ("M-s" . consult-history) ;; next-matching-history-element
	 ("M-r" . consult-history)) ;; previous-matching-history-element
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (consult-customize
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-bookmark
   consult-recent-file
   consult-xref
   consult--source-bookmark
   consult--source-recent-file
   consult--source-project-recent-file
   :preview-key '(:debounce 0.2 any))
  
  ;; Integrate with project.el
  (setq consult-project-function #'consult--default-project-function)
  
  ;; Integrate with projectile
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  
  (setq completion-in-region-function
	(lambda (&rest args)
	  (apply (if vertico-mode
	             #'consult-completion-in-region
	           #'completion--in-region)
	         args)))
  
  (setq consult-narrow-key "<"))

;; Consult enhancements for `project.el`
(use-package consult-project-extra)

;; Consult enhancements for `embark.el`
(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'feature/completing-read)
