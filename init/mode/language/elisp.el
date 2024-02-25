;;; elisp.el --- emacs-lisp-mode -*- lexical-binding: t; -*-

(use-package elisp-mode
  :straight (:type built-in)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-d C-d" . describe-function)
              ("C-c C-d d" . describe-function)
              ("C-c C-k" . eval-buffer))
  :config
  (message "elisp-mode: mode/language/elisp"))

;; https://github.com/abo-abo/lispy
;; (straight-use-package 'lispy)

;; (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

;; enable lispy in M-:
;; (defun conditionally-enable-lispy ()
;;   (when (eq this-command 'eval-expression)
;;     (lispy-mode 1)))
;; (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)

(use-package highlight-defined
  :straight t
  :custom
  (highlight-defined-face-use-itself t)
  :hook
  (help-mode . highlight-defined-mode)
  (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
  :straight t
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

;; (use-package highlight-sexp
;;   :straight
;;   (highlight-sexp :repo "daimrod/highlight-sexp" :fetcher github :version original)
;;   :hook
;;   (clojure-mode . highlight-sexp-mode)
;;   (emacs-lisp-mode . highlight-sexp-mode)
;;   (lisp-mode . highlight-sexp-mode))

;; (use-package eros
;;   :ensure t
;;   :hook
;;   (emacs-lisp-mode . eros-mode))

;; (use-package suggest
;;   :ensure t
;;   :defer t)

(use-package ipretty
  :straight t
  :config
  (ipretty-mode 1))

(use-package outline
  :straight nil
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              ;; prevent `outline-level' from being overwritten by `lispy'
              (setq-local outline-level #'outline-level)
              ;; setup heading regexp specific to `emacs-lisp-mode'
              (setq-local outline-regexp ";;;\\(;* \\)")
              ;; heading alist allows for subtree-like folding
              (setq-local outline-heading-alist
                          '((";;; " . 1)
                            (";;;; " . 2)
                            (";;;;; " . 3)
                            (";;;;;; " . 4)
                            (";;;;;;; " . 5))))))

(provide 'mode/language/elisp)
;;; elisp.el ends here
