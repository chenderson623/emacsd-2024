;;; -*- lexical-binding: t; -*-

;; https://github.com/protesilaos/denote
;; (use-package denote
;;   :straight t
;;   :commands (denote denote-open-or-create denote-link)
;;   :bind
;;   (("C-c n n" . denote)
;;    ("C-c n o" . denote-open-or-create)
;;    ("C-c n i" . denote-link)))

;; https://github.com/protesilaos/denote
(use-package denote
  :ensure t
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
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  ( :map global-map
    ;;("C-c n c" . consult-denote-prefix-map) ;; defined in consult-denote
    ("C-c n d" . denote-dired)
    ("C-c n g" . denote-grep)
    ("C-c n n" . denote)
  )

  :config
  ;; Remember to check the doc string of each of those variables.
  ;;(setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-save-buffers nil)
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-keywords-to-not-infer-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1))

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
  (define-key global-map (kbd "C-c n c") (cons "consult-denote" 'consult-denote-prefix-map))
  :config
  (consult-denote-mode 1))

;; https://github.com/namilus/denote-menu
(use-package denote-menu
  :straight t
  :commands (list-denotes)
  :bind ("C-c n m l" . list-denotes)
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

;; TODO make hydra:
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

(provide 'feature/notetaking)

