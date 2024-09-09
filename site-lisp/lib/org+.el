;;; org+.el --- custom org functions -*- lexical-binding: t; -*-

(defun org+-lib/org-filename-from-title
    (title)
  "Creates a useful filename based on a header string, TITLE.
For instance, given the string:    What's all this then?
     This function will return:    whats-all-this-then"
  (interactive "s")
  (let* ((no-letters (rx (one-or-more (not alphanumeric))))
         (init-try (->> title
                        capitalize
                        (replace-regexp-in-string "'" "")
                        (replace-regexp-in-string no-letters "-"))))
    (string-trim init-try "-+" "-+")))

(defun org+-lib/org-tagsafe (tag-text)
  (upcase (replace-regexp-in-string "-" "" tag-text)))

(provide 'lib/org+)
