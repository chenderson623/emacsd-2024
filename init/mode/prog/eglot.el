;;; eglot.el --- description -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'pretty-hydra)
  )

;; https://www.gnu.org/software/emacs/manual/html_mono/eglot.html
;;  - Features: https://www.gnu.org/software/emacs/manual/html_mono/eglot.html#Eglot-Features
;;  - Git repo: https://github.com/joaotavora/eglot
(use-package eglot
  :straight (:type built-in)
  :commands (eglot-ensure)
  :custom
  (eglot-connect-timeout (* 30 60))
  :pretty-hydra
  ((:color teal :title "Eglot")
   ("Actions"
    (("s" #'eglot "start")
     ("S" #'eglot-reconnect "reconnect")
     ("k" #'eglot-shutdown "eglot shutdown")
     ("K" #'eglot-shutdown-all "eglot shutdown all")
     ("r" #'eglot-rename "rename")
     ("f" #'eglot-format "format")
     ("F" #'eglot-format-buffer "format buffer")
     ("d" #'eglot-find-declaration "declaration")
     ("i" #'eglot-find-implementation "implementations"))
    "Code Actions"
    (("c" #'eglot-code-actions "actions")
     ("o" #'eglot-code-action-organize-imports "organize imports")
     ("q" #'eglot-code-action-quickfix "quickfix")
     ("E" #'eglot-code-action-extract "extract")
     ("I" #'eglot-code-action-inline "inline")
     ("R" #'eglot-code-action-rewrite "rewrite")))))

(provide 'mode/prog/eglot)
;;; eglot.el ends here
