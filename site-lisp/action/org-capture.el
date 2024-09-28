;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun org-capture>make-frame (&optional capture-url)
  "Create a new frame and run org-capture."
  (interactive)
  (message "inside")
  (make-frame '((name . "capture")
                (width . 120)
                (height . 15)))
  (select-frame-by-name "capture")
  (setq word-wrap 1)
  (setq truncate-lines nil)
  (if capture-url (org-protocol-capture capture-url) (org-capture)))

(provide 'action/org-capture)
