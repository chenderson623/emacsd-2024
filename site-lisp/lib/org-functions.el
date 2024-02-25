;;;-*- lexical-binding: t; -*-

;; adopted from https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
;;;###autoload
(defun org-link-delete-link ()
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
(defun copy-id-property ()
  "copy the id property of current heading"
  (interactive)
  (kill-new (org-entry-get (point) "ID")))

;;;###autoload
(defun my>org-filename-from-title
    (title)
  "Creates a useful filename based on a header string, TITLE.
For instance, given the string:    What's all this then?
     This function will return:    whats-all-this-then"
  (interactive "s")
  (let* ((no-letters (rx (one-or-more (not alphanumeric))))
         (init-try (->> title
                        downcase
                        (replace-regexp-in-string "'" "")
                        (replace-regexp-in-string no-letters "-"))))
    (string-trim init-try "-+" "-+")))

(provide 'lib/org-functions)

