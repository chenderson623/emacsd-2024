;;; -*- lexical-binding: t; -*-

;;; Org cache
(setq org-clock-persist-file           (emacs-state*filepath "clock-persist.el"))
(setq org-id-locations-file            (emacs-state*filepath "org-id-locations.el"))
(setq org-persist-directory            (emacs-state*filepath "org-persist"))
(setq org-publish-timestamp-directory  (emacs-state*filepath "org-timestamps"))

;;; Org
(use-package org
  ;;  :straight t
  :straight (:type built-in)
  :custom
  ;; Aesthetics & UI
  (org-catch-invisible-edits 'smart)     ;; prevent editing invisible area
  (org-cycle-separator-lines 0)          ;; no empty lines in collapsed view
  (org-ellipsis "…")                     ;; nicer elipses "↷" "↴" "▼"
  (org-fontify-quote-and-verse-blocks t) ;; make quotes stand out
  (org-hide-emphasis-markers t)          ;; hide emph markers
  (org-hide-leading-stars t)             ;; hide leading stars
  (org-pretty-entities nil)                ;; make latex look good, etc.
  (org-pretty-entities-include-sub-superscripts nil) ;; prettify sub/superscripts
  (org-read-date-prefer-future 'time) ;; Incomplete dates refer to future dates & times
  (org-startup-folded nil)            ;; Don't start org in outline
  (org-tags-column 0)       ;; place tags directly next to headline text
  (org-auto-align-tags nil) ;; don't auto-align tags

  ;; Images
  (org-image-actual-width nil) ;; When non-nil, use the actual width of images when inlining them.
  (org-image-max-width nil) ;; When non-nil, limit the displayed image width. This setting only takes effect when `org-image-actual-width' is set to t or when #+ATTR* is set to t.

  ;; Footnotes
  (org-footnote-section nil)   ;; place footnotes locally
  (org-footnote-auto-adjust t) ;; renumber footnotes

  ;; Indentation
  (org-adapt-indentation t)            ;; adapt indentation
  (org-startup-indented t)             ;; start with indentation of headlines
  (org-indent-indentation-per-level 4) ;; indent 4 spaces
  (org-src-preserve-indentation t) ;; If non-nil preserve leading whitespace characters on export. This fixes src block indentation issues

  ;; Wrapping
  (org-startup-truncated nil) ;; wrap lines

  ;; Insertion/Yanking
  (org-insert-heading-respect-content t) ;; insert new headings after subtree
  (org-M-RET-may-split-line '((default . t))) ;; don't split line when creating a new headline, list item, or table field
  (org-yank-adjusted-subtrees t) ;; adjust subtrees to depth when yanked
  (org-yank-folded-subtrees t)   ;; fold subtrees on yank

  ;; Lists
  (org-list-allow-alphabetical t) ;; allow alphabetical list

  ;; Demote sequence for list bullets
  (org-list-demote-modify-bullet
   '(("+" . "-") ("-" . "+") ("*" . "+")))
  (org-list-indent-offset 1) ;; increase sub-item indentation

  ;; Logging
  (org-log-done 'time)    ;; add timestamp to completed todos
  (org-log-into-drawer t) ;; log state changes into a drawer
  (org-log-state-notes-insert-after-drawers nil)
  (org-log-redeadline nil) ;; don't log the time a task was rescheduled/redeadlined.
  (org-log-reschedule nil)

  ;; Movement
  (org-return-follows-link t) ;; make RET follow links
  (org-special-ctrl-a/e t)    ;; better movement in headers

  ;; Searching
  (org-imenu-depth 4)   ;; scan to depth 8 w/imenu
  (imenu-auto-rescan t) ;; make sure imenu refreshes

  ;; Source block settings
  (org-src-fontify-natively t)         ;; use lang-specific fontification
  (org-src-window-setup 'other-window) ;; edit source in other window
  (org-src-tab-acts-natively t)        ;; use lang bindings
  (org-confirm-babel-evaluate nil)       ;; confirm evaluation

  ;; TODOS
  (org-use-fast-todo-selection 'expert) ;; don't use popup window for todos
  ;; don't set to DONE if children aren’t DONE
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
)

(provide 'mode/org-REWORK)

