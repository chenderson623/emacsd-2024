;;; org.el --- description -*- lexical-binding: t; -*-
;;

;;; Org cache
(setq org-clock-persist-file           (emacs-state*filepath "clock-persist.el"))
(setq org-id-locations-file            (emacs-state*filepath "org-id-locations.el"))
(setq org-persist-directory            (emacs-state*filepath "org-persist"))
(setq org-publish-timestamp-directory  (emacs-state*filepath "org-timestamps"))

;;; Org
(use-package org-super-links
  :straight (org-super-links :type git :host github :repo "toshism/org-super-links" :branch "develop")
  :bind (
         :prefix-map my-org-superlinks-map
         :prefix "C-c L"
         ("s" . org-super-links-link)
         ("l" . org-super-links-store-link)
         ("C-l" . org-super-links-insert-link)))

(use-package org
  ;;  :straight t
  :straight (:type built-in)
  :ensure nil
  :demand t
  :commands (org-mode)
  :mode (("\\.org$" . org-mode))
  :bind (
         :map global-map
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c o" . org-open-at-point-global)
         :prefix-map my-org-link-map
         :prefix "C-c l"
         ("l" . org-insert-link)
         ("s" . org-store-link)
         )

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

  (defun init-org-mode ()
    "Start up org"
    (require 'org)
    (require 'org-tempo)
    )

  (add-hook 'after-init-hook 'init-org-mode)

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

  :config
  (message "configure init/mode/org")
  (require 'elec-pair)
  ;; Open file links in current window, rather than new ones
  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/config.el#L632
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  
  (add-hook 'org-mode-hook (lambda ()  ;; don't pair < symbols
                             (setq-local electric-pair-inhibit-predicate
                                         `(lambda (c)
                                            (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  ;; Setup further org config
  ;;(require 'lem-setup-org-settings)
  ;;(require 'lem-setup-org-extensions))
  )

(use-package org-capture
  :ensure nil
  :requires org
  :commands (org-capture)
  :config
  (load-file (expand-file-name "site-lisp/contrib/org-protocol-capture-html.el" emacsd$dir))
)

(use-package org-protocol
  :ensure nil
  :after org)

(use-package org-crypt
  :after org
  :custom
  (org-tags-exclude-from-inheritance (quote ("crypt")))
  (org-crypt-disable-auto-save nil)
  :config
  (org-crypt-use-before-save-magic))

(use-package org-tempo
  ;;:after org
  :config
  (let ((templates '(
                     ("sh" . "src sh")
                     ("el" . "src emacs-lisp")
                     )))
    (dolist (template templates)
      (push template org-structure-template-alist)))
  )

;; https://github.com/nobiot/org-transclusion
(use-package org-transclusion
  :straight t
  :after org
  :bind ("C-c t" . org-transclusion-add))

;; https://github.com/alphapapa/org-ql
(use-package org-ql
  :straight t
  :commands (org-ql-search org-ql-view org-ql-find)
  :bind
  ("M-g o" . org-ql-find))

(use-package org-refile
  :after org
  :commands org-refile
  :config

  (defadvice org-capture-refile (after save-after-refile-advice activate)
    (org-save-all-org-buffers))

  ;; FROM: https://yiming.dev/blog/2018/03/02/my-org-refile-workflow/
  (defun +org/opened-buffer-files ()
    "Return the list of files currently opened in emacs"
    (delq nil
          (mapcar (lambda (x)
                    (if (and (buffer-file-name x)
                             (not (string= "refile.org" (buffer-file-name x)))
                             (string-match "\\.org$"
                                           (buffer-file-name x)))
                        (buffer-file-name x)))
                  (buffer-list))))

  (setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 4)))
  )

;; Org Agenda Notifications
;; (use-package org-yaap
;;   :ensure nil
;;   :after org
;;   :defer 5
;;   :quelpa (org-yaap :fetcher gitlab :repo "tygrdev/org-yaap")
;;   :custom
;;   (org-yaap-altert-severity 'critical)
;;   (org-yaap-altert-before 10)
;;   :config
;;   (org-yaap-mode 1)
;;   (org-yaap-daemon-start))

;; (with-eval-after-load 'org
;; ;; Load additional org modules
;; (add-to-list 'org-modules 'org-habit t)
;; (add-to-list 'org-modules 'org-tempo t)
;; (add-to-list 'org-modules 'org-protocol t)
;; (when sys-mac
;;   (add-to-list 'org-modules 'org-mac-link t)))

;;;; Org ID
;; Use org ids for reference
;; (use-package org-id
;;   :straight nil
;;   :after org
;;   :custom
;;   (org-id-locations-file (concat lem-cache-dir ".org-id-locations"))
;;   (org-id-method 'ts) ;; use timestamp for id
;;   (org-id-link-to-org-use-id 'create-if-interactive)) ;; create ids

(provide 'mode/org)
;;; org.el ends here
