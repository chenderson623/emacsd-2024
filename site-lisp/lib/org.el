;;; org.el --- custom org functions -*- lexical-binding: t; -*-

(require 'dash)
(require 'cl-lib)

(defun org-lib/org-filename-from-title
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

(defun org-lib/org-tagsafe (tag-text)
  (upcase (replace-regexp-in-string "-" "" tag-text)))

;; This is modified from ha-org-create-org-file
(defun org-lib/create-org-file-from-props (filepath header body tags properties)
    "Create a new Org file by FILEPATH. The contents of the file is
    pre-populated with the HEADER, BODY and any associated TAGS."
    (find-file-other-window filepath)
    (org-set-file-property "TITLE" (read-string "TITLE: " header) t)
    (when tags
      (org-set-file-property "FILETAGS" (s-join " " tags)))

    ;; Insert any drawer properties as #+PROPERTY entries:
    (when properties
      (goto-char (point-min))
      (or (re-search-forward "^\s*$" nil t) (point-max))
      (--map (insert (format "#+PROPERTY: %s %s \n" (cl-first it) (cl-second it))) properties))

    ;; My auto-insert often adds an initial headline for a subtree, and in this
    ;; case, I don't want that... Yeah, this isn't really globally applicable,
    ;; but it shouldn't cause a problem for others.
    (when (re-search-forward "^\\* [0-9]$" nil t)
      (replace-match ""))

    (delete-blank-lines)
    (goto-char (point-max))
    (insert "\n")
    (insert body))


(provide 'lib/org)
