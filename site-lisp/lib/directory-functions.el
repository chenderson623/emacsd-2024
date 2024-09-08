;;; directory-functions.el --- description -*- lexical-binding: t; -*-

(defun create-timestamped-directory (directory-name)
  "Create a new directory with a timestamp prefix and sanitized name."
  (interactive "sEnter directory name: ")
  (let* ((sanitized-name (replace-regexp-in-string "[^A-Za-z0-9_-]" "-" (replace-regexp-in-string " " "_" directory-name)))
         (timestamp (format-time-string "%Y-%m-%d-%H%M%S"))
         (new-directory (concat timestamp "-" sanitized-name)))
    (make-directory new-directory)
    (message "Created directory: %s" new-directory)))


(provide 'lib/directory-functions)
;;; directory-functions.el ends here
