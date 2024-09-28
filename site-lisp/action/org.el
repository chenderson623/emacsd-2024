;; -*- lexical-binding: t -*-

;;;###autoload
(defun og-action>replace-quote-block-with-elisp-block ()
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

;; adopted from https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
;;;###autoload
(defun org-action>delete-url-from-link ()
  "Remove the link part of an org-mode link at point and keep
only the description"
  (interactive)
  (let ((elem (org-element-context)))
    (if (eq (car elem) 'link)
        (let* ((content-begin (org-element-property :contents-begin elem))
               (content-end  (org-element-property :contents-end elem))
               (link-begin (org-element-property :begin elem))
               (link-end (org-element-property :end elem)))
          (if (and content-begin content-end)
              (let ((content (buffer-substring-no-properties content-begin content-end)))
                (delete-region link-begin link-end)
                (insert content)))))))

;; copy property
(defun org-action>copy-id-property ()
  "copy the id property of current heading"
  (interactive)
  (kill-new (org-entry-get (point) "ID")))


(provide 'action/org)
;;; org.el ends here
