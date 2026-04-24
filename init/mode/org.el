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
  ;:commands (org-mode)
  ;:mode (("\\.org$" . org-mode))
  :bind 
  (:map global-map
        ("C-c a" . org-agenda)
        ("C-c c" . org-capture)
        ("C-c o" . org-open-at-point-global)
        :prefix-map my-org-link-map
        :prefix "C-c l"
        ("l" . org-insert-link)
        ("s" . org-store-link))
  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  (org-mode . my/org-font-setup)
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
  ;; (org-tags-column 0)       ;; place tags directly next to headline text
  (org-auto-align-tags t) ;; don't auto-align tags

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
  (org-treat-insert-todo-heading-as-state-change t) ;; inserting a TODO heading is treated as state change
  (org-log-state-notes-insert-after-drawers nil)
  (org-log-redeadline nil) ;; don't log the time a task was rescheduled/redeadlined.
  (org-log-reschedule nil)

  ;; Habit
  (org-habit-show-habits t)
  (org-habit-show-all-today t)
  ;;(org-habit-show-habits-only-for-today t)
  (org-habit-show-done-always-green t)
  (org-habit-graph-column 40)
  (org-habit-preceding-days 28)
  (org-habit-following-days 7)
  
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

  :init
  ;; Org-Emphasis-Regex settings. Set regex boundaries for emphasis.
  ;; Load this before org-mode is loaded.
  ;; See https://emacs.stackexchange.com/q/54673/11934
  (setq org-emphasis-regexp-components
        '
        ("-—[:space:]('\"{["
         "\] - [:space:].,:!?;'\")}\\["
         "[:space:]"
         "."
         1))
  :config
  ;;
  ;;;; load modules
  ;;
  (with-eval-after-load 'org
    ;; Load additional org modules
    (add-to-list 'org-modules 'org-tempo t)
    (add-to-list 'org-modules 'org-habit t)
    (add-to-list 'org-modules 'org-protocol t))

  ;;
  ;;;; elec-pair
  ;;
  (require 'elec-pair)
  
  (add-hook 'org-mode-hook (lambda ()  ;; don't pair < symbols
                             (message "ORG_MODE_HOOK")
                             (setq-local electric-pair-inhibit-predicate
                                         `(lambda (c)
                                            (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

  ;;;; Open file links in current window, rather than new ones
  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/config.el#L632
  ;;(setf (alist-get 'file org-link-frame-setup) #'find-file)
  
  ;; Only load org extensions after opening an org document
  (defun init-org-extensions ()
    (message "init-org-extensions")
    (require 'mode/extended/org-extensions)
    (remove-hook 'org-mode-hook #'init-org-extensions)
    )
  (add-hook 'org-mode-hook #'init-org-extensions)

  (require 'org-protocol)
  (require 'org-capture)

  ;;(my/org-font-setup)
  
  (message "[USE-PACKAGE:config] - ORG"))

(defun my-setup-org-fonts--OLD()
  (let* ((variable-tuple

          (cond ((x-list-fonts "Iosevka Comfy")   '(:font "Iosevka Comfy"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (headline `( :weight bold)))


    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.4))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))
        
     )))

;;   From [[file:/home/chris/collections/emacsd-others/ghoseb--dotemacs/dotemacs/conf/org-config.el::6][org-config.el]]:
(defun my/org-font-setup ()
  "Set faces for heading levels."
  (interactive)
  (dolist (face '((org-level-1 . 1.35)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.15)
                  (org-level-4 . 1.12)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
;;    (set-face-attribute (car face) nil :font bg--variable-pitch-font :weight 'regular :height (cdr face)))

;;  (set-face-attribute 'org-document-title nil :font bg--variable-pitch-font :weight 'bold :height 1.5)

    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))

  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.5)
  
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


;; ---------------------------------------------------
;;
;;;; load at startup
;;
;; ---------------------------------------------------

(use-package org-protocol
  :straight (:type built-in)
  :after org)

(use-package org-capture
  :straight (:type built-in)
  :after org
  :commands (org-capture)
  :config
  (require 'contrib/org-protocol-capture-html))

;; https://github.com/alphapapa/org-ql
(use-package org-ql
  :straight t
  :after org
  :commands (org-ql-search org-ql-view org-ql-find)
  :bind
  ("M-g o" . org-ql-find))

;; https://github.com/unhammer/org-mru-clock
(use-package org-mru-clock
  :straight t
  :after org
  :bind* (("C-c C-x i" . org-mru-clock-in)
          ("C-c C-x j" . org-mru-clock-select-recent-task))
  :config
  (setq org-mru-clock-how-many 100
        org-mru-clock-files #'org-agenda-files)
  (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook))

(use-package org-super-links
  :straight (org-super-links :type git :host github :repo "toshism/org-super-links" :branch "develop")
  :after org
  :bind 
  (:map my-org-link-map
        ("S" . org-super-links-link)
        ("L" . org-super-links-store-link)
        ("C-l" . org-super-links-insert-link)
        ("r" . org-super-links-quick-related)
        )
  :config
  (defun org-super-links-quick-related ()
    (interactive)
    (let ((org-super-links-link-prefix "\nrelated: "))
      (org-super-links-link)))

  (use-package org-super-links-peek
    :straight (org-super-links-peek :type git :host github :repo "toshism/org-super-links-peek" :branch "master")
    :bind 
    (:map my-org-link-map
          ("p" . org-super-links-peek-link)
          ))
  
  )

(require 'org)

(provide 'mode/org)

