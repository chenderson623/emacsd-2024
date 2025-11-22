;; keybindings.el --- Custom Keybindings  -*- lexical-binding: t; -*-

;;;; core emacs keybinding overrides and sensible defaults

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key  "\C-v" nil)
(global-set-key  "\C-z" nil)

(global-set-key  "\C-a"	'crux-move-beginning-of-line)   ; 'move-beginning-of-line

;;(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer)

;;;;; Scale Text
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") 'text-scale-adjust)

(keymap-global-set "M-q" #'sc-reformat-paragraph-or-region)

(provide 'keybindings)
