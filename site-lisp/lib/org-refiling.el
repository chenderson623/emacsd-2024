;;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'ha-boxes-extra)
(require 'cl-lib)

;; This is modified from ha-org-refile-subtree-to-file to send whole body
;; credit to: https://howardism.org/Technical/Emacs/getting-even-more-boxes-done.html
;; TODO rename org-refiling>subtree-to-file
;;;###autoload
(defun my-org-refile-subtree-to-file (dir)
  "Archive the org-mode subtree and create an entry in the
directory folder specified by DIR. It attempts to move as many of
the subtree's properties and other features to the new file."
  (interactive "DDestination: ")
  (let* ((props      (ha-org-subtree-metadata))
         (head       (plist-get props :header))
         (tags       (plist-get props :tags))
         (properties (plist-get props :properties))
         (area       (plist-get props :region))
         (filename   (concat
		      (format-time-string "%Y%m%dT%H%M%S" (org-read-date t t (org-entry-get nil "CREATED" t)))
		      "--"         
         (filepath   (format "%s/%s.org" dir filename))
         (body       (apply #'buffer-substring-no-properties area))
	 )
    (apply #'delete-region area)
    (my-org-refile-create-org-file-from-props filepath head body tags properties)))

;;;###autoload
(defun my-org-refile-copy-subtree-to-file (dir)
  "Archive the org-mode subtree and create an entry in the
directory folder specified by DIR. It attempts to move as many of
the subtree's properties and other features to the new file."
  (interactive "DDestination: ")
  (let* ((props      (ha-org-subtree-metadata))
         (head       (plist-get props :header))
         (tags       (plist-get props :tags))
         (properties (plist-get props :properties))
         (area       (plist-get props :region))
         (filename   (concat
		      (format-time-string "%Y%m%dT%H%M%S" (org-read-date t t (org-entry-get nil "CREATED" t)))
		      "--"         
         (filepath   (format "%s/%s.org" dir filename))
         (body       (apply #'buffer-substring-no-properties area))
	 )
    (my-org-refile-create-org-file-from-props filepath head body tags properties)))

;; This is modified from ha-org-create-org-file
;; TODO rename: org-refiling:create-org-file-from-props
(defun my-org-refile-create-org-file-from-props (filepath header body tags properties)
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

(provide 'lib/org-refiling)
