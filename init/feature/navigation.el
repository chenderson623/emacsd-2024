;;; navigation.el --- Set up navigation features -*- lexical-binding: t; -*-

;;;; Avy
;; https://github.com/abo-abo/avy
(defun avy-action/copy-and-yank (pt)
  "Copy and yank sexp starting on PT."
  (avy-action-copy pt)
  (yank))

(use-package avy
  :straight t
  :bind
  (("C-:" .   avy-goto-char-timer)
   ("C-." .   avy-goto-word-1)
   ("C-c C-j" .   avy-resume)
   ("M-[" . avy-goto-symbol-1-above)
   ("M-]" . avy-goto-symbol-1-below)
   :map isearch-mode-map
   ("C-'" . avy-isearch)
   :map goto-map
   ("M-g" . avy-goto-line)
   :map search-map
   ("M-s" . avy-goto-word-1))
  :config
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq avy-dispatch-alist
        '((?x . avy-action-kill-move)
          (?X . avy-action-kill-stay)
          (?t . avy-action-teleport)
          (?m . avy-action-mark)
          (?c . avy-action-copy)
          (?y . avy-action-yank)
          (?Y . avy-action-yank-line)
          (?i . avy-action-ispell)
          (?z . avy-action-zap-to-char)
          (?p . avy-action/copy-and-yank)))
  
  (setq avy-timeout-seconds 1.0)
  )


;; (defhydra hydra-avy ( :hint nil :quit-key ("q" "C-g"))
;;   "
;;      ^Char^            ^other^
;; -------------------------------------
;; [_c_]   char         [_w_]   word
;; [_C_]   char-2       [_s_]   subword
;; [_t_]   char-timer   [_l_]   line
;; "
;;     ("c" avy-goto-char :exit t)
;;     ("C" avy-goto-char-2 :exit t)
;;     ("t" avy-goto-char-timer :exit t)
;;     ("w" avy-goto-word-1 :exit t)
;;     ("s" avy-goto-subword-1 :exit t)
;;     ("l" avy-goto-line :exit t))


;; You add this to your config to bind some stuff:

;; (avy-setup-default)
;; (global-set-key (kbd "C-c C-j") 'avy-resume)
;; It will bind, for example, avy-isearch to C-' in isearch-mode-map, so that you can select one of the currently visible isearch candidates using avy.

;; this is all it is:
;; (defun avy-setup-default ()
;;   "Setup the default shortcuts."
;;   (eval-after-load "isearch"
;;     '(define-key isearch-mode-map (kbd "C-'") 'avy-isearch)))


;;
;;;; marks
;;
;; Transient-mark-mode supporting push-mark
;; Source: https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

;;(define-key global-map (kbd "C-`") 'push-mark-no-activate)
;;(define-key global-map (kbd "M-`") 'jump-to-mark)


;;
;;;; other window
;;
(defun isearch-forward-other-window (prefix)
  "Function to isearch-forward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-excursion
      (let ((next (if prefix -1 1)))
        (other-window next)
        (isearch-forward)
        (other-window (- next))))))

(defun isearch-backward-other-window (prefix)
  "Function to isearch-backward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-excursion
      (let ((next (if prefix 1 -1)))
        (other-window next)
        (isearch-backward)
        (other-window (- next))))))

(define-key global-map (kbd "C-M-s") 'isearch-forward-other-window)
(define-key global-map (kbd "C-M-r") 'isearch-backward-other-window)

(define-key global-map (kbd "C-M-V") 'scroll-other-window-down)


(provide 'feature/navigation)
