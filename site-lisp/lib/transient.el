;;; transient.el --- transient functions -*- lexical-binding: t; -*-

;;;###autoload
(defun my/transient-format-toggle (label mode-symbol)
  "Format a transient toggle label for a minor MODE-SYMBOL with LABEL.
Returns a string like \"[X] Label\" or \"[ ] Label\"."
  (format "[%s] %s"
          (if (symbol-value mode-symbol) (propertize "X" 'face 'bold) " ")
          label))


(provide 'lib/transient)
;;; transient.el ends here
