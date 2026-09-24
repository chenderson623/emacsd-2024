;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun my-file:unique-org-filepath (directory filename)
  "Return a unique .org path for FILENAME in DIRECTORY."
  (let ((filepath (expand-file-name (concat filename ".org") directory))
        (suffix 1))
    (while (file-exists-p filepath)
      (setq filepath
            (expand-file-name
             ;; append (n) to filename
             (format "%s(%d).org" filename suffix)
             directory))
      (setq suffix (1+ suffix)))
    filepath))

;;;###autoload
(defun my-file>copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>::<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (buffer-file-name) "::" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

;;;###autoload
(defun my-file>find-file-at-point-with-line()
  "If file has an attached line num goto that line, ie boom.rb:12."
  (interactive)
  (setq line-num 0)
  (save-excursion
    (search-forward-regexp "[^ ]::" (point-max) t)
    (if (looking-at "[0-9]+")
        (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
  ;; (find-file-at-point)
  (find-file (ffap-guesser))
  (if (not (equal line-num 0))
      (goto-line line-num)))

(provide 'lib/file)
;;; file.el ends here
