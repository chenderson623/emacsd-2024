;;; -*- lexical-binding: t; -*-

(use-package ibuffer
  :straight nil
  :commands (ibuffer ibuffer-jump)
  :hook
  ((ibuffer-mode . hl-line-mode)
   ))

(use-package ace-jump-buffer
  :straight t
  :commands (ace-jump-buffer ace-jump-buffer-other-window ace-jump-same-mode-buffers)
    :bind
  (:map goto-map
        ("b" . ace-jump-buffer)))


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
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "^\\*scratch.*\\*$"
          "^\\*Ibuffer\\*" ibuffer-mode
          "Output\\*$"
          help-mode
          compilation-mode))

  (setq popper-display-control nil)
  
  (popper-mode +1))

;; Enforce rules for popups.
;; https://depp.brause.cc/shackle/.
(use-package shackle
    :straight t
    :defer 1
    :commands shackle-mode
  :custom
  (shackle-default-rule '(:select t))
  (shackle-rules
   '((compilation-mode :select nil :size 0.6)
     ("\\`\\*Messages" :select t :align t :size 0.6)
     ("\\`\\*company-coq:" :regexp t :noselect t)
     ("\\`\\*fetch" :regexp t :size 0.25 :noselect t :align bottom)
     ("\\`\\*Flycheck" :regexp t :size 0.2 :noselect t :align bottom)
     ("\\`\\*?magit-diff" :regexp t :align bottom :noselect t)))
  :config
  (shackle-mode 1))

;;; persistent scratch
;; https://github.com/Fanael/persistent-scratch
(use-package persistent-scratch
  :straight t
  :demand t
  :custom
  (persistent-scratch-save-file (emacs-cache*filepath ".persistent-scratch"))
  (persistent-scratch-autosave-interval 60)
  ;;(persistent-scratch-what-to-save '(point narrowing))
  :config
  (persistent-scratch-autosave-mode t)
  (persistent-scratch-setup-default))

(provide 'feature/buffers)

