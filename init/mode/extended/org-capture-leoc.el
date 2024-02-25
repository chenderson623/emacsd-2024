;;; -*- lexical-binding: t; -*-

;; from: https://github.com/leoc/.emacs.d
;; this works pretty good.
;; with this script: emacsclient -c -F "((name . \"emacs-capture\") (height . 20) (width . 100))" -e "(leoc/org-protocol-in-capture-frame \"$@\")"
(use-package org-protocol
  :config
  (progn
    (require 'org-protocol)

    (defun leoc/org-protocol-capture-p ()
      "Return true if this capture was initiated via org-protocol."
      (equal "emacs-capture" (frame-parameter nil 'name)))
    (defun leoc/org-capture-delete-frame ()
      "Delete frame if capture was initiated via org-protocol."
      (when (leoc/org-protocol-capture-p)
        (delete-frame)))

    (defun leoc/org-capture-delete-other-windows ()
      "Make sure frame has only one window if capture was initiated via org-protocol."
      (when (leoc/org-protocol-capture-p)
        (delete-other-windows)))

    (add-hook 'org-capture-mode-hook 'leoc/org-capture-delete-other-windows)
    (add-hook 'org-capture-after-finalize-hook 'leoc/org-capture-delete-frame)

    (defadvice org-switch-to-buffer-other-window (after org-capture-supress-window-splitting activate)
      "Delete the extra window if we're in a capture frame."
      (leoc/org-capture-delete-other-windows))

    (defun leoc/org-protocol-in-capture-frame (&optional capture-url)
      "Create a new frame and run org-protocol."
      (interactive)
      (select-frame-by-name "emacs-capture")
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (message "%S" capture-url)
      (if (and capture-url
               (eq 0 (length capture-url)))
          (org-capture)
        (org-protocol-check-filename-for-protocol capture-url t nil)))

    ;;; Disable all advices and hooks for debugging:
    ;; (setq org-capture-mode-hook nil)
    ;; (setq org-capture-after-finalize-hook nil)
    ;; (advice-remove 'org-capture-supress-window-splitting 'org-capture)
    ;; (advice-remove 'org-capture-protocol-ignore-error 'org-capture)
    ;; (ad-deactivate 'org-protocol-capture-html--with-pandoc)
    ;; (advice-remove 'org-protocol-capture-new-window 'org-protocol-capture-html--with-pandoc)
    ))


(provide 'mode/extended/org-capture)
