;;; -*- lexical-binding: t; -*-

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
   ("(" . dired-hide-details-mode)
   (")" . dired-hide-details-mode))
  :custom
  ;; -a :: all
  ;; -g :: like -l, but do not list owner
  ;; -h :: print sizes like 1K 234M 2G etc.
  ;; -o :: like -l, but do not list group information
  ;; -A :: do not list implied . and ..
  (dired-listing-switches "-ahgo --group-directories-first")
  ;; prevent opening extra dired buffers
  (dired-kill-when-opening-new-dired-buffer t)
  ;; If there are two dired open side by side, copy destination is the other.
  ;; M-n shows other candidates including the current directory.
  (dired-dwim-target t)
  :config
  ;;
  ;;; dired-recent.el
  ;; https://github.com/vifon/dired-recent.el/tree/22104c87593f24ec513dfdf97fc4c8c91defec33
  ;; Press C-x C-d to select a previously visited directory to open.
  (use-package dired-recent
    :straight t
    :demand t
    :config
    (setq dired-recent-directories-file (emacs-state*filepath "dired-recent"))
    (dired-recent-mode 1))

  (defun init-dired ()
    (message "dired-before-readin-hook")
    (require 'mode/extended/dired)
    (remove-hook 'dired-before-readin-hook #'init-dired)
    )
  (add-hook 'dired-before-readin-hook #'init-dired)

  ;; when isearching in dired, enter will selected the file as well as
  ;; ending the isearch
  (defun quit-isearch-and-pass-return-to-dired()
    (when (and (eq major-mode 'dired-mode)
                         (not isearch-mode-end-hook-quit)
                         (eq last-input-event 'return)) ; <==========
                (dired-find-file)))
  (add-hook 'isearch-mode-end-hook #'quit-isearch-and-pass-return-to-dired)  
  )

(provide 'mode/dired)
