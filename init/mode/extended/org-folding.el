;;; -*- lexical-binding: t; -*-

;; https://github.com/nullman/emacs-org-visibility
(use-package org-visibility
  :straight t
  :after (org)
  ;;:bind (:map org-visibility-mode-map
  ;;             ("C-x C-v" . org-visibility-force-save) ; defaults to `find-alternative-file'
  ;;             ("C-x M-v" . org-visibility-remove))    ; defaults to undefined
  :hook (org-mode . org-visibility-mode)
  :custom
  ;; optionally change the location of the state file
  (org-visibility-state-file `,(emacs-state*filepath "org-visibility-state"))
  ;; list of directories and files to persist and restore visibility state of
  ;; (org-visibility-include-paths `(,(file-truename "~/.emacs.d/init-emacs.org")
  ;;                                 ,(file-truename "~/org")))
  ;; persist all org files regardless of location
  (org-visibility-include-regexps '("\\.org\\'"))
  ;; list of directories and files to not persist and restore visibility state of
  ;;(org-visibility-exclude-paths `(,(file-truename "~/org/old")))
  ;; optionally set maximum number of files to keep track of
  ;; oldest files will be removed from the state file first
  (org-visibility-maximum-tracked-files 100)
  ;; optionally set maximum number of days (since saved) to keep track of
  ;; files older than this number of days will be removed from the state file
  (org-visibility-maximum-tracked-days 180)
  ;; optionally turn off visibility state change messages
  ;;(org-visibility-display-messages nil)
  )

(provide 'mode/extended/org-folding)
