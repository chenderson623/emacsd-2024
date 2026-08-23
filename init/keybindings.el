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

;;;; org mode map
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-v y") #'org+>copy-org-block-contents))


(defvar my$my-leader-key "M-m"
  "My global leader key")

(defvar-keymap my$leader-prefix-map
  :doc "My global leader key map."
  "w" (cons "Writing Mode" #'hrs-writing-mode)
  "v" (cons "Variable Font Mode" #'my>variable-pitch-buffer-face-mode)

  ;; TODO make "s" be spelling keymap

  "s" (cons "Spellcheck Menu" #'my-transient>spellcheck-menu)
  "T" (cons "Toggles Menu" #'my-transient>toggle-menu)

  "f" #'find-file

  )

(keymap-set global-map my$my-leader-key my$leader-prefix-map)

;; (use-package transient
;;   :straight (:type built-in)
;;   ;;:defer 5
;;   :config
;;   (transient-define-prefix my-transient>toggle-menu ()
;;     "My prefix transient menu for minor modes."
;;     ["Toggles"
;;      ("l" display-line-numbers-mode :description (lambda () (my/transient-format-toggle "Line numbers" 'display-line-numbers-mode)) :transient t)
;;      ("h" hl-line-mode :description (lambda () (my/transient-format-toggle "Highlight line" 'hl-line-mode)) :transient t)])

;;   )

(provide 'keybindings)
