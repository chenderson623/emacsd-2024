;;; -*- lexical-binding: t; -*-

;; https://github.com/protesilaos/denote
(use-package denote
  :straight t
  :commands (denote denote-open-or-create denote-link)
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
  :init
  (define-prefix-command 'my-denote-prefix-map)
  (define-key global-map (kbd "C-c n") 'my-denote-prefix-map)
  (define-key my-denote-prefix-map (kbd "d") 'denote>sort-dired-modified-time)
  (define-key my-denote-prefix-map (kbd "g") 'denote-grep)
  (define-key my-denote-prefix-map (kbd "n") 'denote)
  (define-key my-denote-prefix-map (kbd "o") 'denote-open-or-create)
  (define-key my-denote-prefix-map (kbd "l") 'denote-link)
  (define-key my-denote-prefix-map (kbd "c") 'denote-link-after-creating)

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
            ;; (dired (denote-directories))
            (denote-sort-dired ".*" 'last-modified nil nil)
            )
  )

;; https://github.com/protesilaos/consult-denote
;; https://protesilaos.com/emacs/consult-denote
(use-package consult-denote
  :straight t
  :bind (
         :map consult-denote-prefix-map
         ("f" . consult-denote-find)
         ("g" . consult-denote-grep))
  :custom
  (consult-denote-find-command #'consult-fd)
  :init 
  (define-prefix-command 'consult-denote-prefix-map)
  (define-key my-denote-prefix-map (kbd "s") 'consult-denote-prefix-map)
  :config
  (consult-denote-mode 1))

;; https://github.com/namilus/denote-menu
(use-package denote-menu
  :straight t
  :commands (list-denotes)
  :bind (
         :map denote-menu-prefix-map
         ("l" . list-denotes))
  :init 
  (define-prefix-command 'denote-menu-prefix-map)
  (define-key my-denote-prefix-map (kbd "m") 'denote-menu-prefix-map)
  :config
  (define-key denote-menu-mode-map (kbd "c") #'denote-menu-clear-filters)
  (define-key denote-menu-mode-map (kbd "/ r") #'denote-menu-filter)
  (define-key denote-menu-mode-map (kbd "/ k") #'denote-menu-filter-by-keyword)
  (define-key denote-menu-mode-map (kbd "/ o") #'denote-menu-filter-out-keyword)
  (define-key denote-menu-mode-map (kbd "e") #'denote-menu-export-to-dired))

;; https://github.com/protesilaos/denote-org
;; Typing C-c C-x C-u (org-dblock-update) with point on that line runs (or re-runs) the associated function with the given parameters and populates the block's contents accordingly
(use-package denote-org
  :straight t
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
      (let ((tags (org-get-tags))
            (date (denote-org--get-heading-date))
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

(pretty-hydra-define hydra-jump-dir
  (:title (pretty-hydra-title "Jump to directory" 'octicon "nf-oct-file_directory_open_fill") :quit-key ("C-g" "q" "<escape>") :all-exit t)
  ("Base"
   (("t" trashed "Trashed")
    ("d" consult-dir "Dirs")

    ("n"  (lambda ()
            (interactive)
            ;; (dired (denote-directories))
            (denote-sort-dired ".*" denote-sort-dired-default-sort-component t nil)
            ) "Denote-Dir")

    ("v" (lambda ()
           (interactive)
           (when user/dirvish
             (call-interactively #'dirvish-dwim))) "Dirvish"))

   "Search"
   (("s s" (lambda ()
             (interactive)
             (autoload 'consult-fd-dir "init-func" nil t)
             (consult-fd-dir)) "Fuzzy search dir HOME")
    ("s n" consult-notes "Fuzzy search dir Note")
    ("s d" consult-denote-find "Fuzzy search dir Denote")
    ("j" dired-jump "Dired jump")
    ("J" dired-jump-other-window "Dired jump other"))))

(require 'transient)
(transient-define-prefix my/links-menu ()
  "Links"
  ["Denote Backlinks"
   ("b" "Buffer" denote-backlinks :transient nil)]
  [["Org Link"
;;    ("c" "Copy IDlink" my/copy-idlink :transient nil)
    ("i" "Insert" org-insert-link :transient nil)
    ("s" "Store" org-store-link :transient nil)
    ("t" "Display" org-toggle-link-display :transient nil)]
   ["Denote Insert"
    ("l" "Link" denote-link :transient nil)
    ("h" "Heading" denote-org-link-to-heading :transient nil)
    ("%" "Links: With Regexp" denote-add-links :transient nil)
    ("d" "Links: DBlock" denote-org-dblock-insert-links :transient nil)
    ("D" "Backlinks: DBlock" denote-org-dblock-insert-backlinks :transient nil)]
   ["Denote Links Roam"
    ;; ("e" "Explore Links" my/denote-find-link-other-window :transient t)
    ("fb" "Find Backlinks" denote-find-backlink :transient nil)
;;    ("fr" "References" citar-denote-find-reference :transient nil)
    ]
   ["Misc"
;;    ("g" "Grab: Safari" my/link-grab :transient nil)
;;    ("x" "Remove" jf/org-link-remove-link :transient nil)
    ]])


(pretty-hydra-define medusa/denote
  (:color blue :quit-key "<escape>" :title "Denote")
  ("Create" (
    ("n" denote "_n_ew note" )
    ("t" denote-type "other _t_ype" )
    ("d" denote-date  "other _d_ate" )
    ("s" denote-subdirectory  "other _s_ubdir" )
    ("T" denote-template  "with _T_emplate" )
    ("S" denote-signature  "with _S_ignature" ))
   "Link" (
    ("l" denote-link-or-create "_l_ink" )
    ;;("L" denote-link-or-create-with-command "_L_ink with command" )
    ("h" denote-org-link-to-heading  "specific _h_eader" )
    ("r" denote-add-links "by _r_egexp" )
    ("d" denote-add-links "by _d_ired" )
    ("b" denote-backlinks "_b_acklinks" ))
   "Rename" (
    ("RF" denote-rename-file "Rename File")
    ("FT" denote-change-file-type-and-front-matter  "only FileType")
    ("UF" denote-rename-file-using-front-matter "use Frontmatter"))
   "Dyn. Block" (
    ("DL" denote-org-dblock-insert-links "dyn. Links" )
    ("DB" denote-org-dblock-insert-backlinks "dyn. Backlinks" ))
   "Convert links" (
    ("CF" denote-org-convert-links-to-file-type "to File Type" )
    ("CD" denote-org-convert-links-to-denote-type "to Denote Type" ))
  "Other" (
     ("?" (info "denote") "Help")
     ;;("M-SPC" major-mode-hydra "Major Mode Hydra")
     )))

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
    ("UF" "Use Frontmatter" denote-rename-file-using-front-matter)]]
  [["Dyn. Block"
    ("DL" "Dyn. Links" denote-org-dblock-insert-links)
    ("DB" "Dyn. Backlinks" denote-org-dblock-insert-backlinks)]
   ["Convert links"
    ("CF" "To File Type" denote-org-convert-links-to-file-type)
    ("CD" "To Denote Type" denote-org-convert-links-to-denote-type)]
   ["Settings & Other"
    ("c" denote-set-directory)
    ("?" "Help" (lambda () (interactive) (info "denote")))]])


;; If you intend to use Denote with a variety of file types, it is
;; easier to bind the link-related commands to the `global-map', as
;; shown here.  Otherwise follow the same pattern for `org-mode-map',
;; `markdown-mode-map', and/or `text-mode-map'.
;; ("C-c n l" . denote-link)
;; ("C-c n L" . denote-add-links)
;; ("C-c n b" . denote-backlinks)
;; ("C-c n q c" . denote-query-contents-link) ; create link that triggers a grep
;; ("C-c n q f" . denote-query-filenames-link) ; create link that triggers a dired
;; ;; Note that `denote-rename-file' can work from any context, not just
;; ;; Dired bufffers.  That is why we bind it here to the `global-map'.
;; ("C-c n r" . denote-rename-file)
;; ("C-c n R" . denote-rename-file-using-front-matter)

;; ;; Key bindings specifically for Dired.
;; :map dired-mode-map
;; ("C-c C-d C-i" . denote-dired-link-marked-notes)
;; ("C-c C-d C-r" . denote-dired-rename-files)
;; ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
;; ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter)

  ;; ("C-c n i" . denote-link-or-create)
  ;; ("C-c n c" . denote-open-or-create)
  ;; ("C-c n b" . denote-find-backlink)
  ;; ("C-c n d" . denote-date)
  ;; ("C-c n l" . denote-find-link)
  ;; ("C-c n h" . denote-org-extras-link-to-heading)

(provide 'feature/notetaking)
