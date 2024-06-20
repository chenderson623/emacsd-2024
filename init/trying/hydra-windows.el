;;; hydra-windows.el --- description -*- lexical-binding: t; -*-

(straight-use-package 'ace-window)

(global-set-key
 (kbd "C-M-o")
 (defhydra hydra-window (:color amaranth)
   "window"
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
        "vert")
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
        "horz")
   ("t" transpose-frame "'")
   ("o" delete-other-windows "one" :color blue)
   ("a" ace-window "ace")
   ("s" ace-swap-window "swap")
   ("d" ace-delete-window "del")
   ("i" ace-maximize-window "ace-one" :color blue)
   ("b" ido-switch-buffer "buf")
   ("m" headlong-bookmark-jump "bmk")
   ("q" nil "cancel")))

(provide 'hydra-windows)
;;; hydra-windows.el ends here
