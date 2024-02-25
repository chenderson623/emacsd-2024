;;; -*- lexical-binding: t; -*-


;;;; discover-my-major
;; https://framagit.org/steckerhalter/discover-my-major
(straight-use-package 'discover-my-major)

;;;; helpful
;; https://github.com/Wilfred/helpful
(use-package helpful
  :bind
  ("M-." . helpful-at-point)
  ("s-h" . my/helpful-menu)
  :straight t
  :config
  (transient-define-prefix my/helpful-menu ()
    "Return a `transient' compliant list to apply to different transients."
    ["Help"
     ""
     ("Q" "Kill Helpful Buffers" helpful-kill-buffers)
     ""
     ("b" "Bindings" embark-bindings)
     ("c" "Command" helpful-command)
     ("f" "Function (interactive)" helpful-callable)
     ("F" "Function (all)" helpful-function)
     ("k" "Key" helpful-key)
     ("l" "Library" find-library)
     ("m" "Macro" helpful-macro)
     ("p" "Thing at point" helpful-at-point)
     ("." "Thing at point" helpful-at-point)
     ("t" "Text properties" describe-text-properties)
     ("v" "Variable" helpful-variable)]))

(provide 'feature/discoverability)

