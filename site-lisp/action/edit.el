;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun edit-action>move-to-char (arg char)
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Move to char: " t)))
  (search-forward (char-to-string char) nil nil arg))

;;;###autoload
(defun edit-action>mark-to-char-exclusive (arg char)
  "Mark up to but not including ARGth occurrence of CHAR."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Mark to char: " t)))
  (set-mark
   (save-excursion
     (edit-action>move-to-char arg char)
     (backward-char)
     (point))))

;;;###autoload
(defun edit-action>mark-to-char-inclusive (arg char)
  "Mark up to and including ARGth occurrence of CHAR."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Mark to char: " t)))
  (set-mark
   (save-excursion
     (move-to-char arg char)
     (point))))

;;;###autoload
(defun edit-action>move-region-to-other-window (start end)
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


(provide 'action/editing)

