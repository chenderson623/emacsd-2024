;;; denote-ui.el --- Denote hydras and transient (lazy) -*- lexical-binding: t; -*-

(require 'pretty-hydra)
(require 'transient)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Hydras
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pretty-hydra-define my/denote-create-note-hydra
  (:color blue :quit-key "<escape>" :title "Create Denote Note [%s(abbreviate-file-name denote-directory)]")
  ("Standard" (
    ("n" denote "New note")
    ("o" denote-open-or-create "Open or create"))
   "Custom" (
    ("t" denote-type "Other type")
    ("d" denote-date "Other date")
    ("s" denote-subdirectory "Other subdir")
    ("T" denote-template "With template")
    ("S" denote-signature "With signature"))
   "Silo" (
    ("c" denote-silo-create-note "Create in silo")
    ("O" denote-silo-open-or-create "Open or create in silo"))
   "Settings" (
    ("." (lambda ()
           (interactive)
           (call-interactively #'my>denote-choose-directory)
           (list-denotes)) "Choose directory" :exit nil))))

(pretty-hydra-define my/denote-org-hydra
  (:color blue :quit-key "<escape>" :title "Denote Org Commands [%s(abbreviate-file-name denote-directory)]")
  ("Extraction" (
    ("e" my/denote-org-extract-org-subtree "Extract subtree (with CAPTURED date)")
    ("E" my>denote-org-copy-org-subtree "Copy subtree"))
   "Links" (
    ("l" denote-org-link-to-heading "Link to heading")
    ("b" denote-org-backlinks-for-heading "Backlinks for heading")
    ("cf" denote-org-convert-links-to-file-type "Convert to file type")
    ("cd" denote-org-convert-links-to-denote-type "Convert to denote type"))
   "Dynamic Blocks" (
    ("df" denote-org-dblock-insert-files "Insert files")
    ("dl" denote-org-dblock-insert-links "Insert links")
    ("db" denote-org-dblock-insert-backlinks "Insert backlinks")
    ("dm" denote-org-dblock-insert-missing-links "Insert missing links")
    ("dh" denote-org-dblock-insert-files-as-headings "Insert files as headings"))
   "Settings" (
    ("." (lambda ()
           (interactive)
           (call-interactively #'my>denote-choose-directory)
           (dired denote-directory)) "Choose directory" :exit nil))))

(pretty-hydra-define my/denote-note-hydra
  (:color blue :quit-key "<escape>" :title "Denote: Inside Note [%s(abbreviate-file-name denote-directory)]")
  ("Links" (
    ("l" denote-link "Insert link")
    ("c" denote-link-after-creating "Link after creating")
    ("A" denote-add-links "Add multiple links")
    ("b" denote-backlinks "View backlinks"))
   "Navigate" (
    ("f" denote-find-link "Find link forward")
    ("B" denote-find-backlink "Find backlink backward")
    ("s" consult-denote-find "Search all notes")
    ("g" consult-denote-grep "Grep all notes"))
   "Rename & Metadata" (
    ("r" denote-rename-file "Rename file")
    ("R" denote-rename-file-using-front-matter "Rename using front-matter")
    ("t" denote-change-file-type-and-front-matter "Change file type")
    ("D" my/denote-change-date-and-rename "Change date and rename (prompt)")
    ("C" my/denote-change-date-from-captured "Change date from :CAPTURED: property"))
   "Settings" (
    ("." my>denote-choose-directory "Choose directory" :exit nil))))

(pretty-hydra-define my/denote-menu-hydra
  (:color blue :quit-key "<escape>" :title "Denote Menu Mode [%s(abbreviate-file-name denote-directory)]"
   :body-pre (unless (derived-mode-p 'denote-menu-mode)
               (list-denotes)))
  ("Filter" (
    ("r" denote-menu-filter "Filter by regexp")
    ("k" denote-menu-filter-by-keyword "Filter by keyword")
    ("o" denote-menu-filter-out-keyword "Filter out keyword")
    ("c" denote-menu-clear-filters "Clear filters"))
   "Actions" (
    ("e" denote-menu-export-to-dired "Export to Dired"))
   "Settings" (
    ("." my>denote-choose-directory "Choose directory" :exit nil))))

(pretty-hydra-define my/denote-dired-hydra
  (:color blue :quit-key "<escape>" :title "Denote Dired Mode [%s(abbreviate-file-name denote-directory)]"
   :body-pre (unless (derived-mode-p 'dired-mode)
               (dired denote-directory)))
  ("Rename Marked" (
    ("r" denote-dired-rename-files "Rename files")
    ("k" denote-dired-rename-marked-files-with-keywords "Rename with keywords")
    ("R" denote-dired-rename-marked-files-using-front-matter "Rename using front-matter"))
   "Link" (
    ("l" denote-dired-link-marked-notes "Link marked notes"))
   "Settings" (
    ("." my>denote-choose-directory "Choose directory" :exit nil))))

(pretty-hydra-define my/denote-find-hydra
  (:color blue :quit-key "<escape>" :title "Find & Open Denote Notes [%s(abbreviate-file-name denote-directory)]")
  ("Consult Search" (
    ("f" consult-denote-find "Find file")
    ("g" consult-denote-grep "Grep in notes")
    ("n" consult-notes "Consult notes dir")
    ("s" consult-notes-search-in-all-notes "Search in all notes"))
   "Open / Create" (
    ("o" denote-open-or-create "Open or create")
    ("O" denote-silo-open-or-create "Open or create in silo"))
   "Explore Links" (
    ("l" denote-find-link "Find link forward")
    ("b" denote-find-backlink "Find backlink backward"))
   "Directories & Menus" (
    ("m" list-denotes "List denotes (Menu)")
    ("d" denote>sort-dired-modified-time "Open Dired (Sorted)")
    ("D" denote-silo-dired "Open Silo Dired"))
   "Settings" (
    ("." my>denote-choose-directory "Choose directory" :exit nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Transient Menu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(transient-define-suffix denote-set-directory ()
  :description (lambda () (format "Directory: %s" denote-directory))
  :transient t
  (interactive)
  (let ((dir (read-directory-name "Denote directory: " denote-directory)))
    (setq denote-directory dir)
    (message "Denote directory set to %s" dir)))

(transient-define-prefix my/denote-transient-menu ()
  "Denote"
  [["Create"
    ("n" "New note" denote)
    ("t" "Other type" denote-type)
    ("d" "Other date" denote-date)
    ("s" "Other subdir" denote-subdirectory)
    ("T" "With template" denote-template)
    ("S" "With signature" denote-signature)]
   ["Link"
    ("l" "Link" denote-link-or-create)
    ("h" "Specific header" denote-org-link-to-heading)
    ("r" "By regexp" denote-add-links)
    ("d" "By dired" denote-add-links)
    ("b" "Backlinks" denote-backlinks)]
   ["Rename"
    ("RF" "Rename File" denote-rename-file)
    ("FT" "Only FileType" denote-change-file-type-and-front-matter)
    ("UF" "Use Frontmatter" denote-rename-file-using-front-matter)
    ("RD" "Rename Date (prompt)" my/denote-change-date-and-rename)
    ("RC" "Rename Date (from :CAPTURED:)" my/denote-change-date-from-captured)]]
  [["Dyn. Block"
    ("DL" "Dyn. Links" denote-org-dblock-insert-links)
    ("DB" "Dyn. Backlinks" denote-org-dblock-insert-backlinks)]
   ["Convert links"
    ("CF" "To File Type" denote-org-convert-links-to-file-type)
    ("CD" "To Denote Type" denote-org-convert-links-to-denote-type)]
   ["Settings & Other"
    ("c" denote-set-directory)
    ("?" "Help" (lambda () (interactive) (info "denote")))]])

(provide 'feature/denote-ui)
;;; denote-ui.el ends here
