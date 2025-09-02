;;; treesit.el --- description -*- lexical-binding: t; -*-

(message "TREESIT LOADED")

(use-package treesit
  :straight (:type built-in)
  :init
  ;; where to look for 'tree-sitter directory
  (setq treesit-extra-load-path (list (user-shared*filepath "tree-sitter")))
  
  ;; Make sure `treesit-install-language-grammar' download library file at `treesit-extra-load-path'
  (setq treesit--install-language-grammar-out-dir-history treesit-extra-load-path))

  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (java . ("https://github.com/tree-sitter/tree-sitter-java.git"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (julia . ("https://github.com/tree-sitter/tree-sitter-julia"))
          (R . ("https://github.com/r-lib/tree-sitter-r"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php" nil "php/src"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src" "typescript"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))))
  :config
  (message "USE-PACKAGE:CONFIG: treesit")
  

;; install parsers : `tree-sitter-langs-install-grammars'
;; (use-package tree-sitter-langs
;;   :straight t
;;   :commands (tree-sitter-langs-install-grammars))

(provide 'mode/prog/treesit)
;;; treesit.el ends here
