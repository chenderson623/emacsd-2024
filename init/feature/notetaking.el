;;; -*- lexical-binding: t; -*-

(use-package denote
  :straight (denote :type git :host github :repo "protesilaos/denote" :branch "main")
  :commands (denote denote-open-or-create denote-link)
  :bind
  (("C-c n n" . denote)
   ("C-c n o" . denote-open-or-create)
   ("C-c n i" . denote-link)))

;; https://github.com/protesilaos/consult-denote
;; https://protesilaos.com/emacs/consult-denote
(use-package consult-denote
  :straight t
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :custom
  (consult-denote-find-command #'consult-fd)
  :config
  (consult-denote-mode 1))

;; https://github.com/namilus/denote-menu
(use-package denote-menu
  :straight t
  :commands (list-denotes)
  :bind ("C-c n l" . list-denotes)
  :config
  (define-key denote-menu-mode-map (kbd "c") #'denote-menu-clear-filters)
  (define-key denote-menu-mode-map (kbd "/ r") #'denote-menu-filter)
  (define-key denote-menu-mode-map (kbd "/ k") #'denote-menu-filter-by-keyword)
  (define-key denote-menu-mode-map (kbd "/ o") #'denote-menu-filter-out-keyword)
  (define-key denote-menu-mode-map (kbd "e") #'denote-menu-export-to-dired))

(provide 'feature/notetaking)

