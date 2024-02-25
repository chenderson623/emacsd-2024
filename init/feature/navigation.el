;;; navigation.el --- Set up navigation features -*- lexical-binding: t; -*-

;;;; Avy
;; https://github.com/abo-abo/avy
(use-package avy
  :straight t
  :bind
  (("C-:" .   avy-goto-char-timer)
   ("C-." .   avy-goto-word-1)
   ("C-c C-j" .   avy-resume)
   :map isearch-mode-map
   ("C-'" . avy-isearch)
   :map goto-map
   ("M-g" . avy-goto-line)
   :map search-map
   ("M-s" . avy-goto-word-1)))


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


(provide 'feature/navigation)
