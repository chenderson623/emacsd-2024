;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun move-to-char (arg char)
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Move to char: " t)))
  (search-forward (char-to-string char) nil nil arg))

;;;###autoload
(defun mark-to-char-exclusive (arg char)
  "Mark up to but not including ARGth occurrence of CHAR."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Mark to char: " t)))
  (set-mark
   (save-excursion
     (move-to-char arg char)
     (backward-char)
     (point))))

;;;###autoload
(defun mark-to-char-inclusive (arg char)
  "Mark up to and including ARGth occurrence of CHAR."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Mark to char: " t)))
  (set-mark
   (save-excursion
     (move-to-char arg char)
     (point))))

(provide 'action/editing)

