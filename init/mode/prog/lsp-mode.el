;;; lsp-mode.el --- description -*- lexical-binding: t; -*-

(use-package lsp-mode
  :straight t
  :commands lsp
  :bind ((:map lsp-mode-map
               ("C-c p r" . lsp-rename)
               ("M-'" . lsp-goto-implementation)))
  :init
  (setq gc-cons-threshold (* 100 1024 1024))
  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  (setq lsp-keymap-prefix "C-z")

  :config
  (setq lsp-server-install-dir (emacs-cache*filepath "lsp/"))
  (setq lsp-session-file (emacs-cache*filepath ".lsp-session-v1"))

  ;; (which-key-add-key-based-replacements "C-z F" "Workspace folders")
  ;; (which-key-add-key-based-replacements "C-z G" "Peek")
  ;; (which-key-add-key-based-replacements "C-z g" "find")
  ;; (which-key-add-key-based-replacements "C-z T" "Toggle")
  ;; (which-key-add-key-based-replacements "C-z w" "session")

  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  
  ;; (company-backend-for-hook 'lsp-mode-hook '((company-capf :with company-yasnippet)))
  
  ;; (setq lsp-modeline-code-actions-segments '(name icon))
  ;; (setq lsp-signature-function 'lsp-signature-posframe)

  
  ;; (setq lsp-disabled-clients '(intelephense))

  ;;(setq-default lsp-enable-imenu t)
  ;; (setq lsp-enable-file-watchers nil
  ;;       lsp-enable-folding nil
  ;;       lsp-headerline-breadcrumb-icons-enable nil
  ;;       lsp-headerline-breadcrumb-enable nil
  ;;       lsp-enable-text-document-color nil
  ;;       lsp-log-io nil
  ;;       lsp-enable-on-type-formatting nil
  ;;       lsp-enable-symbol-highlighting nil
  ;;       lsp-keep-workspace-alive nil
  ;;       lsp-enable-dap-auto-configure nil ; temporary issue with dap https://github.com/emacs-lsp/dap-mode/issues/825
  ;;       ;; lsp-enable-on-type-formatting nil
  ;;       lsp-ui-doc-enable nil
  ;;       ;; lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
  ;;       )
  
  ;; (setq lsp-enable-indentation nil
  ;;       lsp-enable-on-type-formatting nil))
  

  )

(use-package lsp-treemacs
  :straight t
  :after lsp-mode 
  :hook (treemacs-select-hook . lsp-treemacs-sync-mode)
  :bind (:map lsp-mode-map ("C-c p e" . lsp-treemacs-errors-list))
  )

(use-package consult-lsp
  :straight t
  :after lsp-mode 
  :bind (:map lsp-mode-map ("M-<return>" . consult-lsp-symbols))
  )

;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :straight t
  :after lsp-mode 
  :hook (lsp-mode . lsp-ui-mode)
  ;;  :bind (:map lsp-mode-map
  ;;              ("C-c t u" . lsp-ui-mode))
  :ensure t
  :config

  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable t
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil
        lsp-completion-provider :none)

  )

(provide 'mode/prog/lsp-mode)
;;; lsp-mode.el ends here
