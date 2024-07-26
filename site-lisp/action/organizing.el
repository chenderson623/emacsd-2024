;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun organizing>move-region-or-subtree-to-other-window ()
  (interactive)
  (when (and
         (eq 'org-mode major-mode)
         (not (region-active-p)))
    (org-mark-subtree))
  (call-interactively 'organizing:move-region-to-other-window))

(defun organizing:move-region-to-other-window (start end)
  "Move selected text to other window"
  (interactive "r")
  (if (use-region-p)
      (let ((count (count-words-region start end)))
        (save-excursion
          (kill-region start end)
(newline)
(other-window 1)
          (yank)
          (newline))
        (other-window -1)
        (message "Moved %s words" count))
    (message "No region selected")))

(provide 'action/organizing)
