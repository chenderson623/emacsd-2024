;;; core-settings.el --- description -*- lexical-binding: t; -*-
;;
;; core emacs settings
;;
;;; Code:


;;;; bell (terminal.c)
(setq
 ring-bell-function 'ignore
 visible-bell t)

;;;; bindings
(setq column-number-indicator-zero-based nil)

;;;; saveplace
;; https://www.emacswiki.org/emacs/SavePlace
(use-package saveplace
  :unless noninteractive
  :hook (after-init . save-place-mode)
  :custom
  (save-place-file (emacs-state*filepath "places"))
  :config
  (save-place-mode 1))

;;;; Bookmarks
(setq bookmark-default-file (emacsd-local-config*filepath "bookmark-default.el"))

;;;; Transient
(setq transient-history-file (emacs-state*filepath "transient/history.el"))
(setq transient-levels-file  (emacs-state*filepath "transient/levels.el"))
(setq transient-values-file  (emacs-state*filepath "transient/values.el"))


;; (use-package mule
;;   :defer 0.1
;;   :config
;;   (prefer-coding-system 'utf-8)
;;   (set-language-environment "UTF-8")
;;   (set-terminal-coding-system 'utf-8))

;; (use-package mule
;;   :defer t
;;   :custom
;;   ;; mule-cmds.el
;;   (current-language-environment "UTF-8")
;;   :config
;;   (set-terminal-coding-system 'utf-8)
;;   (prefer-coding-system 'utf-8)
;;   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))


;;;; Use UTF-8
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8) 
(set-terminal-coding-system  'utf-8)   
(set-keyboard-coding-system  'utf-8)   
(set-selection-coding-system 'utf-8)   
(prefer-coding-system        'utf-8)   
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(when (equal system-type 'windows-nt)
 (setenv "LANG" "en_US.utf8"))

(setq
 ;; (apropos) Non-nil means apropos commands will search more extensively.
 apropos-do-all t
 ;; If non-nil, mouse yank commands yank at point instead of at click (mouse)
 mouse-yank-at-point t
 ;; Whether to add a newline automatically at the end of the file (files)
 require-final-newline t
 ;; Non-nil means load prefers the newest version of a file (lread)
 load-prefer-newer t
 ;; Whether frames should be resized implicitly. (frame)
 frame-inhibit-implied-resize t)

;;;; Scrolling
(setq auto-window-vscroll nil)
;; the text cursor moves off-screen. Instead, only scroll the minimum amount
;; necessary to show the new line. (A number of 101+ disables re-centering.)
(setq scroll-conservatively 101)

(setq scroll-margin 4)

;; Optimize mouse wheel scrolling for smooth-scrolling trackpad use.
;; Trackpads send a lot more scroll events than regular mouse wheels,
;; so the scroll amount and acceleration must be tuned to smooth it out.
(setq
 ;; If the frame contains multiple windows, scroll the one under the cursor
 ;; instead of the one that currently has keyboard focus.
 mouse-wheel-follow-mouse 't
 ;; Completely disable mouse wheel acceleration to avoid speeding away.
 mouse-wheel-progressive-speed nil
 ;; The most important setting of all! Make each scroll-event move 2 lines at
 ;; a time (instead of 5 at default). Simply hold down shift to move twice as
 ;; fast, or hold down control to move 3x as fast. Perfect for trackpads.
 mouse-wheel-scroll-amount '(2 ((shift) . 4) ((control) . 6)))

;; When we split open a new window, we usually want to jump to the new window.
(advice-add #'split-window-below :after (lambda (&rest _) (other-window 1)))
(advice-add #'split-window-right :after (lambda (&rest _) (other-window 1)))

(use-package emacs
  :straight nil
  :defer 1
  :config
  (setq tab-width 4)
  (setq-default fill-column 80)
  (setq fill-column 80)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default tab-always-indent 'complete))

(use-package simple 
  :straight nil
  :no-require t
  :config
  ;; Whether to save existing clipboard text into kill ring before replacing it.
  (setq save-interprogram-paste-before-kill t)
  ;; Indentation can insert tabs if this is non-nil.
  (setq-default indent-tabs-mode nil)
  ;;
  ;;; Mode line
  ;; Toggle column number display in the mode line
  (column-number-mode 1)
  ;; Toggle buffer size display in the mode line
  (size-indication-mode 1))

;;;; savehist
;; Save minibuffer history
;; help:savehist-mode
(use-package savehist
  :straight nil
  :init
  (setq savehist-file (emacs-cache*filepath "savehist.el"))
  (setq savehist-save-minibuffer-history 1)
  (setq savehist-additional-variables
	'(kill-ring
          search-ring
          regexp-search-ring))

  (savehist-mode 1))

;;;; delsel
;; help:delete-selection-mode
(use-package delsel
  :defer 1
  :straight nil
  :no-require t
  :config
  ;; delete seleted text when typing
  (delete-selection-mode 1))

;;;; autorevert
;; auto revert mode [[help:global-auto-revert-mode]]
(use-package autorevert
  :straight nil
  :no-require t
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-reverse-verbose nil)
  (global-auto-revert-non-file-buffers t))

;;;; recent files
;; help:recentf-mode
(use-package recentf
  :straight nil
  :hook (after-init . recentf-mode)
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (setq recentf-save-file (emacs-state*filepath "recentf")
        recentf-max-menu-items 25
        recentf-max-saved-items 100
        recentf-auto-cleanup 300
        recentf-exclude '("~/collections/")))

;;;; file backups and autosaves
(use-package files
  :straight nil
  :no-require t
  :custom
  ;;; backups
  (backup-directory-alist `((".*" . ,(emacs-state*filepath "backups"))))
  (version-control t)
  (kept-new-versions 6)
  (kept-old-versions 10)
  (delete-old-versions t)  
  (backup-by-copying t)

  ;;; autosaves
  (auto-save-file-name-transforms `((".*" ,(concat (filepath:ensure-dir-exists (emacs-state*filepath "auto-save-list")) "/\\1") t)))
  (auto-save-list-file-prefix (emacs-state*filepath "auto-save-list/"))

  ;;;; Auto-save every minute or 300 events
  (auto-save-interval 300)
  (auto-save-timeout 60)

  ;;;; Always auto-save buffers.
  (auto-save-default t)

  ;;;; enable view-mode for read-only files
  (view-read-only t))

;;;; help-at-pt
(use-package help-at-pt
  :straight (:type built-in)
  :custom
  (help-at-pt-display-when-idle t))

;;;; elec-pair
;; Toggle automatic parens pairing (Electric Pair mode).
;; [[help:electric-pair-mode]]
(use-package elec-pair
  :straight nil
  :no-require t
  ;; TODO move prog-mode hooks to prog setup, or filetype setup
  :hook ((prog-mode org-mode) . electric-pair-mode)
  :config
  (setq electric-pair-preserve-balance t
        electric-pair-skip-whitespace nil
        electric-pair-delete-adjacent-pairs t
        electric-pair-open-newline-between-pairs nil
        electric-pair-skip-whitespace-chars '(9 10 32)
        electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-pair-pairs '( ; make electric-pair-mode work on more brackets.
                              (?\{ . ?\})
                              (?\[ . ?\])
                              )))

;;;; paren 
;; Toggle visualization of matching parens
;; [[help:show-paren-mode]]
(use-package paren
  :straight nil
  :no-require t
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-style `mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(provide 'emacs/core-settings)
;;; emacs-settings.el ends here
