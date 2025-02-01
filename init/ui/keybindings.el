;; -*- lexical-binding: t; -*-

;;; Key bindings

;;;; which-key
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :straight t
  :demand t
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.1)
  (which-key-mode 1))

(use-package key-chord
  :straight t
  :defer 10
  :config
  (setq key-chord-one-key-delay 0.16)
  (setq key-chord-two-keys-delay 0.02)
  (key-chord-define-global "uu" 'undo)
  (key-chord-define-global "kk" 'kill-whole-line)
  (key-chord-define-global "jj" 'avy-goto-char-2)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-mode 1))

(use-package hydra
  :straight t
  :commands (defhydra))

(use-package repeat
  :straight nil
  :demand t
  :config
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t)
  (repeat-mode 1)

  (defvar isearch-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "s") #'isearch-repeat-forward)
      (define-key map (kbd "r") #'isearch-repeat-backward)
      map))

  (dolist (cmd '(isearch-repeat-forward isearch-repeat-backward))
    (put cmd 'repeat-map 'isearch-repeat-map))

  (defvar buffer-navigation-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'next-line)
      (define-key map (kbd "p") #'previous-line)
      (define-key map (kbd "f") #'forward-word)
      (define-key map (kbd "b") #'backward-word)
      (define-key map (kbd "u") #'scroll-up-command)
      (define-key map (kbd "d") #'scroll-down-command)
      map))

  (dolist (cmd '(next-line previous-line forward-word backward-word scroll-up-command scroll-down-command))
    (put cmd 'repeat-map 'buffer-navigation-map)))

(provide 'ui/keybindings)
