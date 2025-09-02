;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun file-action>copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>::<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (buffer-file-name) "::" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

;;;###autoload
(defun file-action>find-file-at-point-with-line()
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

(provide 'action/file)
;;; file.el ends here
