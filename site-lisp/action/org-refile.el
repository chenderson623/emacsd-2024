;;; -*- lexical-binding: t; -*-

(require 'lib/org)
(require 'dash)
(require 'contrib/ha-boxes-extra)
(require 'cl-lib)

;; This is modified from ha-org-refile-subtree-to-file to send whole body
;; credit to: https://howardism.org/Technical/Emacs/getting-even-more-boxes-done.html
;;;###autoload
(defun org-refile>refile-subtree-to-file (dir)
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
		      (org-lib/org-filename-from-title head)))
         (filepath   (format "%s/%s.org" dir filename))
         (body       (apply #'buffer-substring-no-properties area))
	 )
    (apply #'delete-region area)
    (org-lib/create-org-file-from-props filepath head body tags properties)))

;;;###autoload
(defun org-refile>copy-subtree-to-file (dir)
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
		      (my>org-filename-from-title head)))
         (filepath   (format "%s/%s.org" dir filename))
         (body       (apply #'buffer-substring-no-properties area))
	 )
    (org-lib/create-org-file-from-props filepath head body tags properties)))

;;;###autoload
(defun org-refile>/refile-to-file-headline (file headline)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))))

;;;###autoload
(defun org-refile>/refile-to-file-headline-reverse (file headline)
  (let ((org-reverse-note-order t)
        (pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))))

(provide 'action/org-refile)
