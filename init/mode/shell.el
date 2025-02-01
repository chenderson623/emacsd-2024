;;; -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns x))
  :demand t
  :config
  (exec-path-from-shell-initialize))

(provide 'mode/shell)

