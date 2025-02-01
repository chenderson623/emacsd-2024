;;; -*- lexical-binding: t; -*-

(use-package treesit
  ;; built in
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((css . ("https://github.com/tree-sitter/tree-sitter-css"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go"))
               (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json"))
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (python . ("https://github.com/tree-sitter/tree-sitter-python"))
               (rust "https://github.com/tree-sitter/tree-sitter-rust")
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional. Combobulate works in both xxxx-ts-modes and
  ;; non-ts-modes.

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  )

(provide 'mode/treesit)
