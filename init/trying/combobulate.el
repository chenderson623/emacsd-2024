;;; combobulate.el --- description -*- lexical-binding: t; -*-

(use-package combobulate
  :straight t
    :custom
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (combobulate-key-prefix "C-c o")
    :hook ((prog-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    ;;:load-path ("path-to-git-checkout-of-combobulate")
    )


(provide 'trying/combobulate)
;;; combobulate.el ends here
