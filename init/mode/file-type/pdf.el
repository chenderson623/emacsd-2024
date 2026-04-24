;;; pdf.el --- description -*- lexical-binding: t; -*-

;;
;;; Init hook for pdf files
;;
(defun pdf:pdf-mode-init()
  (message "HOOK: pdf:pdf-mode-init")
  )
  ;; hook set in filetype-modes.el: (add-hook 'json-ts-mode-hook #'json:json-mode-init)

(message "init pdf file-type")

;;* PDF-Tools
;; need to run M-x pdf-tools-install after the first install to set up the server
;; (use-package pdf-tools
;;   :straight t
;;   :defer t
;; ;;  :mode ("\\.pdf\\'" . pdf-view-mode)
;; ;;  :mode ("\\.PDF\\'" . pdf-view-mode)
;;   :custom
;;   (pdf-view-display-size 'fit-width)
;;   ;; more fine-grained zooming
;;   (pdf-view-resize-factor 1.05)
;;   :bind (:map pdf-view-mode-map
;;           ("C-s" . isearch-forward)
;;           ("C-r" . isearch-backward))
;;   :config
;;   (pdf-tools-install)
;;   (pdf-loader-install)
;;   )
;; ;; works but 1st pdf loaded does not render

(use-package pdf-tools
  :straight t
  :commands (pdf-tools-install)
  )

(pdf-tools-install)  ; Standard activation command
(pdf-loader-install) ; On demand loading, leads to faster startup time

;; (use-package pdf-tools
;;   :mode (("\\.pdf$" . pdf-view-mode))
;;   :mode (("\\.PDF$" . pdf-view-mode))
;;   :commands (pdf-view-mode)
;;   :init
;;   (pdf-loader-install :no-query)
;;   :bind (:map pdf-view-mode-map
;;               ;; Navigation
;;               ("j"  . pdf-view-next-line-or-next-page)
;;               ("k"  . pdf-view-previous-line-or-previous-page)
;;               ("l"  . pdf-view-next-page-command)
;;               ("h"  . pdf-view-previous-page-command)
;;               ("g"  . pdf-view-first-page)
;; q              ("G"  . pdf-view-last-page)
;;               ("t"  . pdf-view-goto-page)
;;               ("l"  . pdf-view-goto-label)
;;               ("s-c"  . pdf-view-kill-ring-save)
;;               ("S"    . xah-open-in-external-app)
;;               ("s-p"  . dwim-shell-command-print)
;;               ;; Search
;;               ("/"  . isearch-forward)
;;               ("?"  . isearch-backward)
;;               ;; Actions
;;               ("-"  . pdf-view-shrink)
;;               ("+"  . pdf-view-enlarge)
;;               ("="  . pdf-view-fit-page-to-window)
;;               ("r"  . pdf-view-revert-buffer)
;;               ("o"  . pdf-links-action-perform)
;;               ("O"  . pdf-outline)
;;               ("!"  . my-pdf-no-filter))
;;   :config
;;   ;; initialise
;;   ;;(pdf-tools-install-noverify)
;;   ;; HiDPI
;;   ;;(setq pdf-view-use-imagemagick t
;;   ;;      pdf-view-use-scaling t)

;;   (defun my-pdf-no-filter ()
;;     "View pdf without colour filter."
;;     (interactive)
;;     (pdf-view-midnight-minor-mode -1))

;;   ;; tex hook
;;   ;; see https://github.com/politza/pdf-tools#auto-revert
;;   ;;(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
;;   ;; other hooks
;;   ;; disable ctrl-f, which conflicts
;;   ;;(add-hook 'pdf-isearch-minor-mode-hook (lambda () (ctrlf-local-mode -1)))
;;   ;;(add-hook 'pdf-isearch-minor-mode-hook (lambda () (ctrlf-local-mode -1)))
;;   (add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)
;;   (add-hook 'pdf-view-mode-hook (lambda ()
;;                                   (blink-cursor-mode -1)
;;                                   (line-number-mode -1)
;;                                   (display-line-numbers-mode -1)
;;                                   (column-number-mode -1)
;;                                   (auto-revert-mode -1)))
;;   )

(use-package org-noter
  :straight t
)


(provide 'mode/file-type/pdf)
