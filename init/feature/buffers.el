;;; -*- lexical-binding: t; -*-

(use-package ibuffer
  :straight nil
  :commands (ibuffer ibuffer-jump)
  :hook
  ((ibuffer-mode . hl-line-mode)
   ))

;;; popper 
;; https://github.com/karthink/popper
;;
;; Popper is a minor-mode to tame the flood of ephemeral windows Emacs
;; produces, while still keeping them within arm's reach. Designate any
;; buffer to "popup" status, and it will stay out of your way. Disimss
;; or summon it easily with one key. Cycle through all your "popups" or
;; just the ones relevant to your current buffer. Useful for many
;; things, including toggling display of REPLs, documentation,
;; compilation or shell output, etc.
(use-package popper
  :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "^\\*scratch.*\\*$"
          "Output\\*$"
          help-mode
          compilation-mode))
  (popper-mode +1))

(provide 'feature/buffers)

