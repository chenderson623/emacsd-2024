;;; toggles-transient.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'my-tmenu>toggles "lib/toggles" nil t)
(transient-define-prefix my-tmenu>toggles ()
   "My prefix transient menu for minor modes."
    ["Toggles"
     ("l" display-line-numbers-mode :description (lambda () (my/transient-format-toggle "Line numbers" 'display-line-numbers-mode)) :transient t)
     ("h" hl-line-mode :description (lambda () (my/transient-format-toggle "Highlight line" 'hl-line-mode)) :transient t)])

(provide 'lib/toggles-transient)
