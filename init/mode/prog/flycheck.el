;;; flycheck.el --- description -*- lexical-binding: t; -*-

(use-package flycheck
  :straight t
  :diminish
  :commands (flycheck-redefine-standard-error-levels flycheck-add-mode)
  ;; Enable global flycheck
  ;;:hook (after-init . global-flycheck-mode)
  :init
  ;;(setq flycheck-keymap-prefix "")
  (which-key-add-key-based-replacements
    "C-c !" "Flycheck")  

  (setq flycheck-global-modes
        '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
              org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode)
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-indication-mode (if (display-graphic-p)
                                     'right-fringe
                                   'right-margin)
        ;; Only check while saving and opening files
        flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  ;; Prettify indication styles
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (flycheck-redefine-standard-error-levels "⏴" 'flycheck-fringe-bitmap-arrow)

  ;; List errors in bottom window
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))  
  )

(use-package consult-flycheck
  :straight t
  :after (consult flycheck))


;; (defvar flycheck-command-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "c"         #'flycheck-buffer)
;;     (define-key map "C"         #'flycheck-clear)
;;     (define-key map (kbd "C-c") #'flycheck-compile)
;;     (define-key map "n"         #'flycheck-next-error)
;;     (define-key map "p"         #'flycheck-previous-error)
;;     (define-key map "l"         #'flycheck-list-errors)
;;     (define-key map (kbd "C-w") #'flycheck-copy-errors-as-kill)
;;     (define-key map "s"         #'flycheck-select-checker)
;;     (define-key map "?"         #'flycheck-describe-checker)
;;     (define-key map "h"         #'flycheck-display-error-at-point)
;;     (define-key map "e"         #'flycheck-explain-error-at-point)
;;     (define-key map "H"         #'display-local-help)
;;     (define-key map "i"         #'flycheck-manual)
;;     (define-key map "V"         #'flycheck-version)
;;     (define-key map "v"         #'flycheck-verify-setup)
;;     (define-key map "x"         #'flycheck-disable-checker)
;;     map)
;;   "Keymap of Flycheck interactive commands.")

;; (defcustom flycheck-keymap-prefix (kbd "C-c !")
;;   "Prefix for key bindings of Flycheck.

;; Changing this variable outside Customize does not have any
;; effect.  To change the keymap prefix from Lisp, you need to
;; explicitly re-define the prefix key:

;;     (define-key flycheck-mode-map flycheck-keymap-prefix nil)
;;     (setq flycheck-keymap-prefix (kbd \"C-c f\"))
;;     (define-key flycheck-mode-map flycheck-keymap-prefix
;;                 flycheck-command-map)


(provide 'mode/prog/flycheck)
;;; flycheck.el ends here
