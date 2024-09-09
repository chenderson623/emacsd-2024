;; -*- lexical-binding: t -*-

;;;###autoload
(defun og-action/replace-quote-block-with-elisp-block ()
  (interactive)
  (er/mark-org-code-block)
  (replace-string-in-region "#+BEGIN_QUOTE" "#+BEGIN_SRC emacs-lisp")
  (replace-string-in-region "#+END_QUOTE" "#+END_SRC")
)

;;;###autoload
(defun org-action>move-region-or-subtree-to-other-window ()
  (interactive)
  (when (and
         (eq 'org-mode major-mode)
         (not (region-active-p)))
    (org-mark-subtree))
  (call-interactively 'organizing:move-region-to-other-window))

(provide 'action/org)
;;; org.el ends here
