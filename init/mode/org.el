;;; -*- lexical-binding: t; -*-

;;; Org cache
(setq org-clock-persist-file           (emacs-state*filepath "org/clock-persist.el"))
(setq org-id-locations-file            (emacs-state*filepath "org/id-locations.el"))
(setq org-persist-directory            (emacs-state*filepath "org-persist"))
(setq org-publish-timestamp-directory  (emacs-state*filepath "org-timestamps"))

;;; Org
(use-package org
  ;;:hook (after-init-hook . org-mode)
  ;; :defer t
  :straight t
  ;; :pin gnu ; ensure the newest version from 'org' repository
  :config
  (setq
   ;; appearance					
   org-startup-indented t	       	; virtual indentation		
   org-indent-indentation-per-level 4   ; indent 4 spaces
   org-startup-truncated nil            ; wrap lines
   org-image-actual-width nil 
   )
  (require 'org-capture)
  (require 'org-protocol)

  ;; define behavior of org-open-at-point
  (setq org-link-frame-setup
	'((vm . vm-visit-folder-other-frame)
	  (vm-imap . vm-visit-imap-folder-other-frame)
	  (gnus . org-gnus-no-new-news)
	  (file . find-file)
	  (wl . wl-other-frame)))
  
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil)


  )


;; Org Babel
(straight-use-package 'php-mode)
(straight-use-package 'ob-php)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (shell . t)
   (php . t)
   ))

;; bring easy-templates backspace
(require 'org-tempo)

;; add some additional easy-templates
(add-to-list 'org-structure-template-alist
     '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist
     '("S" . "src sh"))

;; State sequences
(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "DOING(o!)" "|" "DONE(d!)" "ARCHIVE(A)")
        (sequence "IDEATE(s!)" "REFINE(w@/!)" "NEXT")
        (sequence "MEETING(m!)" "|" "OVER(o@/!)")
        (sequence "SOMEDAY(s!)" "WAITING(w@/!)" "|" "CANCELED(c@/!)")
        (sequence "TO-READ(r!)" "READING(R!)" "|" "HAVE-READ(d@)")
	))

;; Fix for org-journal not unfolding https://github.com/bastibe/org-journal/issues/392
;;(setq org-fold-core-style 'overlays)

;; log state changes into a drawer
(setq org-log-into-drawer t)

;; add timestamp to completed todos
(setq org-log-done 'time)



(define-key global-map "\C-c a" 'org-agenda)
(define-key global-map "\C-c c" 'org-capture)

;; Start flyspell in org mode buffers
;;(add-to-list 'org-mode-hook 'flyspell-mode)

;; FROM: https://yiming.dev/blog/2018/03/02/my-org-refile-workflow/
(defun +org/opened-buffer-files ()
  "Return the list of files currently opened in emacs"
  (delq nil
        (mapcar (lambda (x)
                  (if (and (buffer-file-name x)
                           (string-match "\\.org$"
                                         (buffer-file-name x)))
                      (buffer-file-name x)))
                (buffer-list))))

(setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 4)))


(defun init-org-mode ()
  "Start up org"
  (require 'org))

(add-hook 'after-init-hook 'init-org-mode)

(provide 'mode/org)
;;; org.el ends here
