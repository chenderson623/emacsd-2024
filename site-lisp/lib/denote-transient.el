;;; denote-transient.el --- transient menus for Denote -*- lexical-binding: t; -*-

(transient-define-suffix tsuffix/denote-set-directory ()
  :description (lambda () (format "Directory: %s" denote-directory))
  :transient t
  (interactive)
  (my>denote-choose-directory))

(defun my/denote-current-file-p ()
  (and buffer-file-name
       (file-regular-p buffer-file-name)
       (denote-file-is-in-denote-directory-p buffer-file-name)
       (denote-file-has-denoted-filename-p buffer-file-name)))

(transient-define-group my/denote-navigation-group
  [:class transient-row
   ("?" "Denote Help" (lambda () (interactive) (info "denote")))
   ("q" "Quit" transient-quit-all)])

;;;###autoload (autoload 'transient-menu/denote "lib/denote-transient" nil t)
(transient-define-prefix transient-menu/denote ()
  "Denote Transient Menu"
  [[
    ("." tsuffix/denote-set-directory)
    ]]
  [["Notes"
    ("n" "New note (C-c n n)" denote)
    ("o" "Open or Create (C-c n o)" denote-open-or-create)
    ]
   ["Find"
    ("f" "Consult find" consult-denote-find)
    ("c" "Consult notes" consult-notes)
    ("g" "Denote Grep" denote-grep)
    ("s" "Consult search notes" consult-notes-search-in-all-notes)
    ]
   ["Dired"
    ("d" "Dired - sorted by modified" denote>sort-dired-modified-time)
    ]
   ["Rename"
    :if my/denote-current-file-p
     ("RF" "Rename File" denote-rename-file)
     ("FT" "Only FileType" denote-change-file-type-and-front-matter)
     ("UF" "Use Frontmatter" denote-rename-file-using-front-matter)
     ("RD" "Rename Date (prompt)" my/denote-change-date-and-rename)
     ("RC" "Rename Date (from :CAPTURED:)" my/denote-change-date-from-captured)]
   ]
  my/denote-navigation-group

  ;; [["Create"
  ;;   ("n" "New note" denote)
  ;;   ("t" "Other type" denote-type)
  ;;   ("d" "Other date" denote-date)
  ;;   ("s" "Other subdir" denote-subdirectory)
  ;;   ("T" "With template" denote-template)
  ;;   ("S" "With signature" denote-signature)]
  ;;  ["Link"
  ;;   ("l" "Link" denote-link-or-create)
  ;;   ("h" "Specific header" denote-org-link-to-heading)
  ;;   ("r" "By regexp" denote-add-links)
  ;;   ("d" "By dired" denote-add-links)
  ;;   ("b" "Backlinks" denote-backlinks)]
  ;;  ["Rename"
  ;;   ("RF" "Rename File" denote-rename-file)
  ;;   ("FT" "Only FileType" denote-change-file-type-and-front-matter)
  ;;   ("UF" "Use Frontmatter" denote-rename-file-using-front-matter)
  ;;   ("RD" "Rename Date (prompt)" my/denote-change-date-and-rename)
  ;;   ("RC" "Rename Date (from :CAPTURED:)" my/denote-change-date-from-captured)]]
  ;; [["Dyn. Block"
  ;;   ("DL" "Dyn. Links" denote-org-dblock-insert-links)
  ;;   ("DB" "Dyn. Backlinks" denote-org-dblock-insert-backlinks)]
  ;;  ["Convert links"
  ;;   ("CF" "To File Type" denote-org-convert-links-to-file-type)
  ;;   ("CD" "To Denote Type" denote-org-convert-links-to-denote-type)]
  ;;  ["Settings & Other"
  ;;   ("c" denote-set-directory)
  ;;   ("?" "Help" (lambda () (interactive) (info "denote")))]]

  )

;;;###autoload (autoload 'transient-menu/denote-create-custom "lib/denote-transient" nil t)
(transient-define-prefix transient-menu/denote-create-custom ()
  "Denote Create Custom Transient Menu"
  [[
    ("." tsuffix/denote-set-directory)
    ]]
  [["Create Custom"
    ("t" "Prompt file type" denote-type)
    ("d" "Prompt date" denote-date)
    ("s" "Prompt subdir" denote-subdirectory)
    ("T" "With template" denote-template)
    ("S" "With signature" denote-signature)
    ]
  ])

;;;###autoload (autoload 'transient-menu/denote-note "lib/denote-transient" nil t)
(transient-define-prefix transient-menu/denote-note ()
  "Denote Note Actions"
  [
    ["Links"
    ("l" "Insert link" denote-link)
    ("c" "Link after creating" denote-link-after-creating)
    ("A" "Add multiple links" denote-add-links)
    ("b" "View backlinks" denote-backlinks)
   ]
   ["Navigate"
    ("f" "Find link forward" denote-find-link)
    ("B" "Find backlink backward" denote-find-backlink)
    ("s" "Search all notes" consult-denote-find)
    ("g" "Grep all notes" consult-denote-grep)
   ]
   ["Rename & Metadata"
    ("r" "Rename file" denote-rename-file)
    ("R" "Rename using front-matter" denote-rename-file-using-front-matter)
    ("t" "Change file type" denote-change-file-type-and-front-matter)
    ("D" "Change date and rename (prompt)" my/denote-change-date-and-rename)
    ("C" "Change date from :CAPTURED: property" my/denote-change-date-from-captured)
   ]
  ])

(provide 'lib/denote-transient)
;;; denote-transient.el ends here
