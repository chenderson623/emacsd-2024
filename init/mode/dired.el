;;; -*- lexical-binding: t; -*-

;; TODO make use of 'once? All of the dired setup runs before the gui is ever called
;; https://emacs.stackexchange.com/questions/68015/how-can-i-load-dired-only-if-i-actually-use-dired
;; TODO put dired extensions in mode/extend/dired ? (still have a once problem)
(defun init-dired ()
  (message "dired-before-readin-hook")
  ;; (require 'mode/extend/dired)
  (remove-hook 'dired-before-readin-hook #'init-dired)
  )
(add-hook 'dired-before-readin-hook #'init-dired)

;;; Dired
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
;; https://www.emacswiki.org/emacs/DiredMode
(use-package dired
  :straight nil
  :commands (dired)
  :hook
  ((dired-mode . hl-line-mode)
    ;; omit files whose filenames match regexp ‘dired-omit-files’,
    ;; files ending with extensions in ‘dired-omit-extensions’,
    ;; or files listed on lines matching ‘dired-omit-lines’
    (dired-mode . dired-omit-mode)
    ;; When this minor mode is enabled, details such as file ownership and permissions are hidden from view.
    (dired-mode . dired-hide-details-mode))
  ;; auto refresh dired when file changes
  (dired-mode . auto-revert-mode)
  :bind
  (:map
    dired-mode-map ("-" . dired-up-directory)
    ;;("e" . wdired-change-to-wdired-mode)
    ("(" . dired-details-toggle) (")" . dired-details-toggle))
  ("s-d" . make-directory)
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired-and-Find.html
  ("s-f" . find-name-dired)
  ("s-g" . find-grep-dired)
  :custom (dired-listing-switches "-aghoA --group-directories-first")
  ;; prevent opening extra dired buffers
  (dired-kill-when-opening-new-dired-buffer t)
  ;; If there are two dired open side by side, copy destination is the other.
  ;; M-n shows other candidates including the current directory.
  (dired-dwim-target t)
  :config (message "config dired"))

;;; dired-aux.el
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/dired-aux.el
(use-package dired-aux
  :straight nil
  :commands (dired-do-async-shell-command dired-isearch-filenames)
  :bind
  (:map
    dired-mode-map
    ("M-s M-s" . dired-do-async-shell-command)
    ("C-s" . dired-isearch-filenames)))

;;;; dired-hide-dotfiles
;; https://github.com/mattiasb/dired-hide-dotfiles
(use-package dired-hide-dotfiles
  :straight t
  :after dired
  :bind (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

;;;; dired-subtree
;; Insert subdirectories in a tree-like fashion
;; https://github.com/Fuco1/dired-hacks
(use-package dired-subtree
  :straight t
  :after dired
  :bind (:map dired-mode-map ("TAB" . dired-subtree-toggle)))

;;;; dired-narrow
;; https://github.com/Fuco1/dired-hacks
(use-package dired-narrow
  :straight t
  :after dired
  :bind (:map dired-mode-map ("/" . dired-narrow)))

;;;; dired-collapse
;; https://github.com/Fuco1/dired-hacks
;; Flaten display of nested directories with no other content.
(use-package dired-collapse
  :straight t
  :after dired)

;; from https://protesilaos.com/emacs/dired-preview
;; call `dired-preview-mode`
(use-package dired-preview
  :straight (dired-preview
    :type git
    :host github
    :repo "protesilaos/dired-preview"
    :branch "main")
  :after dired)

(use-package disk-usage
  :straight t
  :after dired
  :bind (:map dired-mode-map ("C-c d S" . disk-usage-here)) ;; <---- TODO C-c d is a good dired mode map?
  :init (setq disk-usage--format-files 'file-name-nondirectory))

;;;
;;; dired-recent.el
;; https://github.com/vifon/dired-recent.el/tree/22104c87593f24ec513dfdf97fc4c8c91defec33
;; Press C-x C-d to select a previously visited directory to open.
(use-package dired-recent
  :straight t
  :after dired
  :config
  (setq dired-recent-directories-file (emacs-state*filepath "dired-recent"))
  (dired-recent-mode 1))

(provide 'mode/dired)
