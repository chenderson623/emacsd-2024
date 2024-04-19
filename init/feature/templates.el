
(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :hook (after-init . yas-minor-mode)
  :init (setq yas-verbosity 2)
  :config
  (setq yas-snippet-dirs
      (list (emacsd*filepath "snippets")
        (emacsd-local-config*filepath "local-snippets")
        ))
  (yas-global-mode 1))

(use-package autoinsert
  :straight (:type built-in)
  :defer 2
  :config
  (defun +autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  (auto-insert-mode t)
  (setq auto-insert-directory (emacsd*filepath "templates"))

  (add-to-list 'auto-insert-alist '((sh-mode . "Shell scripts") . ["sh.template" +autoinsert-yas-expand]))
  (add-to-list 'auto-insert-alist '((emacs-lisp-mode . "Elisp source code") . ["el.template" +autoinsert-yas-expand]))

  (setq auto-insert-query nil))

(defhydra hydra-yas (:color blue :hint nil)
  "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
 ^ ^       _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas/global-mode)
  ("m" yas/minor-mode)
  ("a" yas-reload-all))

(provide `feature/templates)
