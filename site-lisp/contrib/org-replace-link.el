;;; -*- lexical-binding: t; -*-

;; From: https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link

;;;###autoload
(defun afs/org-replace-link-by-link-description ()
    "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2) 
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))

;;;###autoload
(defun afs/org-replace-all-links-by-description (&optional start end)
  "Find all org links and replace by their descriptions."
  (interactive
   (if (use-region-p) (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (replace-match (match-string-no-properties 
                        (if (match-end 2) 2 1)))))))

(provide 'org-replace-link)
;;; org-replace-link.el ends here
