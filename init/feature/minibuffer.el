;;; minibuffer.el --- setup minibuffer -*- lexical-binding: t; -*-

;; TODO minibuffer stuff [[file:~/collections/emacsd-others/karthink~.emacs.d/.emacs.d/lisp/setup-minibuffer.el::(use-package minibuffer]]

;;;; Minibuffer config
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons
      (format "[CRM%s] %s"
        (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
        (car args))
      (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;;;; Vertico
;; Vertico provides a performant and minimalistic vertical completion UI based on the default completion system
;; https://github.com/minad/vertico
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :hook (after-init . vertico-mode)
;;  :init
;;  (vertico-mode 1)
  :bind (:map vertico-map
	      ("?" . #'minibuffer-completion-help)
	      ("M-RET" . #'minibuffer-force-complete-and-exit)
	      ("M-TAB" . #'minibuffer-complete))  
  )

;;;; vertico extension: vertico-directory
;; This package is a Vertico extension, which provides Ido-like
;; directory navigation commands.  The commands can be bound in the
;; `vertico-map'.  Furthermore a cleanup function for shadowed file
;; paths is provided
(use-package vertico-directory
  :straight nil
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :after vertico
  :bind (:map vertico-map
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word)
	      ("C-w" . vertico-directory-delete-word)
	      ("RET" . vertico-directory-enter)))


;;;; vertico extension: vertico-repeat
;; This package is a Vertico extension, which enables repetition of
;; Vertico sessions via the `vertico-repeat', `vertico-repeat-previous'
;; and `vertico-repeat-select' commands.  If the repeat commands are
;; called from an existing Vertico minibuffer session, only sessions
;; corresponding to the current minibuffer command are offered via
;; completion.
(use-package vertico-repeat
  :straight nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind
  (("M-R" . vertico-repeat)
   :map vertico-map
   ("M-P" . vertico-repeat-previous)
   ("M-N" . vertico-repeat-next)
   ("S-<prior>" . vertico-repeat-previous)
   ("S-<next>" . vertico-repeat-next)))

;;;; Orderless
;; https://github.com/minad/orderless
;; Emacs completion style that matches multiple regexps in any order
;; A completion style is a back-end for completion and is used from a front-end that provides a completion UI
(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq
    completion-styles '(orderless basic)
    completion-category-defaults nil
    completion-category-overrides '((file (styles partial-completion)))))

;;;; Add annotations to minibuffer completions
(use-package marginalia
  :straight t
  :hook (after-init . marginalia-mode)
  :custom (marginalia-align 'right)
)  


(provide 'feature/selection-vertico)
;;; init-vertico.el ends here
