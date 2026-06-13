;;; -*- lexical-binding: t; -*-

(require 'pretty-hydra)
(require 'transient)

(defvar my$denote-directories
        (list "~/Shared/notes"))

(add-to-list 'savehist-additional-variables 'denote-directory)

(unless (boundp 'denote-directory)
  (setq denote-directory (expand-file-name (car my$denote-directories))))

(defun my>denote-choose-directory ()
  (interactive)
  (let* ((prompt (format "Choose denote directory [%s]: " (abbreviate-file-name denote-directory)))
         (choice (completing-read prompt my$denote-directories nil t)))
    (setq denote-directory (expand-file-name choice))
    (message "Switched denote-directory to %s" denote-directory)))

;; https://github.com/protesilaos/denote
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Denote Base
;;  https://github.com/protesilaos/denote
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package denote
  :straight t
  :commands (denote denote-open-or-create denote-link denote-file-is-note-p)
  :hook
  ( ;; If you use Markdown or plain text files, then you want to make
   ;; the Denote links clickable (Org renders links as buttons right
   ;; away)
   (text-mode . denote-fontify-links-mode-maybe)
   ;; Apply colours to Denote names in Dired.  This applies to all
   ;; directories.  Check `denote-dired-directories' for the specific
   ;; directories you may prefer instead.  Then, instead of
   ;; `denote-dired-mode', use `denote-dired-mode-in-directories'.
   (dired-mode . denote-dired-mode))
  :bind
  (("C-c n" . my-denote-prefix-map)
   :map my-denote-prefix-map
   ("d" . denote>sort-dired-modified-time)
   ("g" . denote-grep)
   ("n" . denote)
   ("o" . denote-open-or-create)
   ("l" . denote-link)
   ("c" . denote-link-after-creating)
   ("m" . my>denote-choose-directory)
   ("C-d" . my/denote-dired-hydra/body)
   ("C-o" . my/denote-find-hydra/body)
   ("C-f" . my/denote-create-note-hydra/body)
   ("C-n" . my/denote-note-hydra/body)
   ("C-m" . my/denote-menu-hydra/body)
   ("C-t" . my/denote-transient-menu)
   )
  :init
  (define-prefix-command 'my-denote-prefix-map nil "Denote")
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-c n" "Denote"
      "C-c n s" "Consult Denote"
      "C-c n m" "Denote Menu"))
  :config
  ;; Remember to check the doc string of each of those variables.
  ;;(setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-save-buffers t)
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-keywords-to-not-infer-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1)
  (defun denote>sort-dired-modified-time ()
            (interactive)
            (dired (denote-directories))
            (denote-sort-dired ".*" 'last-modified nil nil)
            )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; denote-silo
;;; Denote Silo
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package denote-silo
  :straight t
  ;; Bind these commands to key bindings of your choice.
  :commands ( denote-silo-create-note
              denote-silo-open-or-create
              denote-silo-select-silo-then-command
              denote-silo-dired
              denote-silo-cd )
  :config
  ;; Add your silos to this list.  By default, it only includes the
  ;; value of the variable `denote-directory'.
  (setq denote-silo-directories my$denote-directories)
)

;; https://github.com/protesilaos/consult-denote
;; https://protesilaos.com/emacs/consult-denote
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Consult Denote
;;  https://github.com/protesilaos/consult-denote
;;  https://protesilaos.com/emacs/consult-denote
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package consult-denote
  :straight t
  :bind (
         :map my-consult-denote-prefix-map
         ("f" . consult-denote-find)
         ("g" . consult-denote-grep)
         :map my-denote-prefix-map
         ("s" . my-consult-denote-prefix-map))
  :custom
  (consult-denote-find-command #'consult-fd)
  :init 
  (define-prefix-command 'my-consult-denote-prefix-map nil "Consult Denote")
  :config
  (consult-denote-mode 1))

;; https://github.com/namilus/denote-menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Denote Menu
;;  https://github.com/namilus/denote-menu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package denote-menu
  :straight t
  :commands (list-denotes)
  :bind (
         :map my-denote-menu-prefix-map
         ("l" . list-denotes)
         :map my-denote-prefix-map
         ("m" . my-denote-menu-prefix-map)
         :map denote-menu-mode-map
         ("c" . denote-menu-clear-filters)
         ("/ r" . denote-menu-filter)
         ("/ k" . denote-menu-filter-by-keyword)
         ("/ o" . denote-menu-filter-out-keyword)
         ("e" . denote-menu-export-to-dired))
  :init 
  (define-prefix-command 'my-denote-menu-prefix-map nil "Denote Menu"))

;; https://github.com/protesilaos/denote-org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Denote Org
;;  https://github.com/protesilaos/denote-org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typing C-c C-x C-u (org-dblock-update) with point on that line runs (or re-runs) the associated function with the given parameters and populates the block's contents accordingly
(use-package denote-org
  :straight t
  :bind (
        ;;  :map org-mode-map
        ;;  ("C-c n O" . my-denote-org-prefix-map)
         :map my-denote-org-prefix-map
         ("h" . my/denote-org-hydra/body)
         ("e" . denote-org-extract-org-subtree)
         ("M-e" . my>denote-org-copy-org-subtree)
         ("l" . denote-org-link-to-heading)
         ("b" . denote-org-backlinks-for-heading)
         ("c f" . denote-org-convert-links-to-file-type)
         ("c d" . denote-org-convert-links-to-denote-type)
         ("d f" . denote-org-dblock-insert-files)
         ("d l" . denote-org-dblock-insert-links)
         ("d b" . denote-org-dblock-insert-backlinks)
         ("d m" . denote-org-dblock-insert-missing-links)
         ("d h" . denote-org-dblock-insert-files-as-headings))
  :init
  (define-prefix-command 'my-denote-org-prefix-map nil "Denote Org")
  (define-key my-denote-prefix-map (kbd "O")
    '(menu-item "Denote Org" my-denote-org-prefix-map
                :filter (lambda (cmd) (if (derived-mode-p 'org-mode) cmd))))
  (define-key my-denote-prefix-map (kbd "e")
    '(menu-item "" denote-org-extract-org-subtree
                :filter (lambda (cmd) (if (derived-mode-p 'org-mode) cmd))))
  (define-key my-denote-prefix-map (kbd "M-e")
    '(menu-item "" my>denote-org-copy-org-subtree
                :filter (lambda (cmd) (if (derived-mode-p 'org-mode) cmd))))
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-c n O" "Org-mode"
      "C-c n O c" "convert links"
      "C-c n O d" "dynamic blocks"))
  :commands
  ( denote-org-link-to-heading
    denote-org-backlinks-for-heading

    denote-org-extract-org-subtree

    denote-org-convert-links-to-file-type
    denote-org-convert-links-to-denote-type

    denote-org-dblock-insert-files
    denote-org-dblock-insert-links
    denote-org-dblock-insert-backlinks
    denote-org-dblock-insert-missing-links
    denote-org-dblock-insert-files-as-headings))

;; same as denote-org-extract-org-subtree bu COPIES instead of moving
(defun my>denote-org-copy-org-subtree ()
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Headings can only be extracted from Org files"))
  (if-let* ((text (org-get-entry))
            (heading (denote-link-ol-get-heading)))
      (let* ((tags (org-get-tags))
             ;; Try to get date from :CAPTURED: property first
             (captured-date-string (org-entry-get (point) "CAPTURED"))
             (date (if captured-date-string
                         (denote-valid-date-p captured-date-string)
                       (denote-org--get-heading-date))) ; Fallback to heading date
            subdirectory
            signature)
        (dolist (prompt denote-prompts)
          (pcase prompt
            ('keywords (when (not tags) (setq tags (denote-keywords-prompt))))
            ('subdirectory (setq subdirectory (denote-subdirectory-prompt)))
            ('date (when (not date) (setq date (denote-date-prompt))))
            ('signature (setq signature (denote-signature-prompt)))))
        (denote heading tags 'org subdirectory date text signature))
    (user-error "No subtree to extract; aborting")))
;; https://github.com/mclear-tools/consult-notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Consult Notes
;;  https://github.com/mclear-tools/consult-notes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package consult-notes
  :straight t
  :commands (consult-notes
             consult-notes-search-in-all-notes
             consult-notes-org-roam-find-node-relation)
  :config
  ;;(setq consult-notes-file-dir-sources '(("Shared"  ?s  "~/Shared/notes"))) ;; Set notes dir(s), see below
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
;;  (setq consult-notes-org-headings-files '("~/Shared/org/refile.org"
;;                                           "/home/chris/KnightSwift/Knight/org/projects.org"))
  ;;(consult-notes-org-headings-mode)
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  ;; search only for text files in denote dir
  (setq consult-notes-denote-files-function (lambda () (denote-directory-files nil t t))))

;; TODO make hydra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Custom Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/denote-change-date-from-captured ()
  "Change the date in the current Org Denote note's identifier using the :CAPTURED: property.
This function reads the date from the :CAPTURED: property of the current Org entry
and uses it to update the note's identifier and rename the file.
It then calls `my/denote-change-date-and-rename` with the extracted date.
"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org mode buffer."))
  ;; It's possible to be in an Org buffer that is not a Denote file,
  ;; but we still want to try and extract the date if it's present.
  ;; The actual renaming function `my/denote-change-date-and-rename`
  ;; will perform the `denote-file-is-note-p` check.

  (let* ((captured-date-string
          ;; Try to get from the current Org entry first (if point is on a heading)
          (or (org-entry-get (point) "CAPTURED")
              ;; Fallback to top-level properties drawer
              (my/denote-get-top-level-property "CAPTURED")))
         (parsed-date-time nil)) ; This was a duplicate line, removed in previous diff, but still present in the provided file.
    (unless captured-date-string
      (user-error "No :CAPTURED: property found in the current Org entry."))

    (setq parsed-date-time (denote-valid-date-p captured-date-string))

    (unless parsed-date-time
      (user-error "Could not parse date from :CAPTURED: property: %s" captured-date-string))

    (my/denote-change-date-and-rename parsed-date-time)
    (message "Date updated from :CAPTURED: property to %s" (format-time-string "%Y-%m-%d %H:%M:%S" parsed-date-time))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/denote-get-top-level-property (property-name &optional buffer)
  "Search for a top-level Org property PROPERTY-NAME and return its value.
PROPERTY-NAME should be a string, e.g., \"CAPTURED\".
This function searches the entire BUFFER (or current buffer if nil) for the
property within a top-level :PROPERTIES: drawer."
  (save-excursion
    (with-current-buffer (or buffer (current-buffer))
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" nil t)
        (let ((properties-start (point)))
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
            (let ((properties-end (point)))
              (goto-char properties-start)
              (when (re-search-forward (format "^[ \t]*:%s:[ \t]+\\(.*\\)$" (regexp-quote property-name)) properties-end t)
                (match-string 1)))))))))

(defun my/denote-change-date-and-rename (&optional new-date-time)
  "Change the date in the current Denote note's identifier and rename the file.
If NEW-DATE-TIME is provided (a time value), it is used directly.
Otherwise, it prompts the user for a new date and time.

This function operates on the current buffer's file, assuming it is a Denote note.
It reconstructs the file identifier and then calls `denote--rename-file` to perform
the actual renaming."
  (interactive)
  (unless (denote-file-is-note-p (buffer-file-name))
    (user-error "Not in a Denote file. Please open a Denote note to use this function."))

  (let* ((current-file (buffer-file-name))
         (current-identifier (denote-retrieve-filename-identifier current-file))
         (actual-new-time-value
          (cond ; Determine the new time value

           (new-date-time new-date-time)
           ((let ((captured-str (my/denote-get-top-level-property "CAPTURED")))
              (when captured-str
                (denote-valid-date-p captured-str))))
           ((if (and denote-date-prompt-use-org-read-date
                     (require 'org nil :no-error))
                (let* ((time (org-read-date nil t nil "Enter new date and time (YYYY-MM-DD HH:MM): " nil))
                       (org-time-seconds (format-time-string "%S" time))
                       (cur-time-seconds (format-time-string "%S" (current-time))))
                  (if (string-equal "00" org-time-seconds)
                      (time-add time (string-to-number cur-time-seconds))
                    time))
              (parse-time-string (read-string "Enter new date and time (YYYY-MM-DD HH:MM:SS): ")))))))
    (let* ((formatted-new-date (format-time-string denote-date-identifier-format actual-new-time-value))
           (new-identifier (concat formatted-new-date (substring current-identifier 15)))
           (file-type (denote-filetype-heuristics current-file)) ; Get the file type
           (original-buffer-title (denote-retrieve-title-or-filename current-file file-type)) ; Use the correct function
           (current-keywords (denote-extract-keywords-from-path current-file))
           (current-signature (or (denote-retrieve-filename-signature current-file) ""))
           (renamed-file (denote--rename-file current-file
                                              original-buffer-title ; Pass the original title
                                              current-keywords
                                              current-signature
                                              actual-new-time-value
                                              new-identifier)))
      (message "Denote file date changed and renamed to %s" (file-name-nondirectory renamed-file)))))

(defun my/denote-org-extract-org-subtree ()
  "Create new Denote note using the current Org subtree as input.
Remove the subtree from its current file and move its contents into a
new Denote file (a subtree is a heading with all of its contents,
including subheadings).

This custom version prioritizes the date found in the `:CAPTURED:`
property of the Org heading. If not found, it falls back to the
standard `denote-org-extract-org-subtree` date logic (Org properties
like DATE, CREATED, CLOSED, or current time).

For other details, refer to the documentation of `denote-org-extract-org-subtree`."
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Headings can only be extracted from Org files"))
  (if-let* ((text (org-get-entry))
            (heading (denote-link-ol-get-heading)))
      (let* ((tags (org-get-tags))
             ;; Try to get date from :CAPTURED: property first
             (captured-date-string (org-entry-get (point) "CAPTURED"))
             (date (if captured-date-string ; This still uses org-entry-get, which is fine for subtrees
                         (denote-valid-date-p captured-date-string)
                       (denote-org--get-heading-date))) ; Fallback to heading date
            subdirectory
            signature)
        (dolist (prompt denote-prompts)
          (pcase prompt
            ('keywords (when (not tags) (setq tags (denote-keywords-prompt))))
            ('subdirectory (setq subdirectory (denote-subdirectory-prompt)))
            ('date (when (not date) (setq date (denote-date-prompt))))
            ('signature (setq signature (denote-signature-prompt)))))
        (delete-region (org-entry-beginning-position)
                       (save-excursion (org-end-of-subtree t) (point)))
        (denote heading tags 'org subdirectory date text signature))
    (user-error "No subtree to extract; aborting")))
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

(provide 'feature/notetaking)
