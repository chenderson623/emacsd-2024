;;; -*- lexical-binding: t; -*-

;; ---------------------------------------------------
;;
;;;; modules
;;
;; ---------------------------------------------------

;; https://orgmode.org/manual/Structure-Templates.html
(use-package org-tempo
  :straight (:type built-in)
  :after org
  :config
  (message "[USE-PACKAGE:config] - ORG TEMPO")
  (let ((templates '(
                     ("sh" . "src sh")
                     ("el" . "src emacs-lisp")
                     ("sq" . "src sql")
                     ("js" . "src javascript")
                     ("jn" . "src json")
                     ("ja" . "src java")
                     ("ya" . "src yaml")
                     ("ph" . "src php")
                     )))
    (dolist (template templates)
      (push template org-structure-template-alist)))
  )

;;;  Lazy load languages
(use-package ob-core
  :straight (:type built-in)
  :after org
  :config
  (defun my/org-babel-execute-src-block (&optional _arg info _params)
    "Load language if needed"
    (let* ((lang (or (nth 0 info) ""))
           (sym (if (member (downcase lang) '("c" "cpp" "c++")) 'C (intern lang)))
           (backup-languages org-babel-load-languages))
      (unless (assoc sym backup-languages)
        (condition-case err
            (progn
              (org-babel-do-load-languages 'org-babel-load-languages (list (cons sym t)))
              (setq-default org-babel-load-languages (append (list (cons sym t)) backup-languages)))
          (file-missing
           (setq-default org-babel-load-languages backup-languages)
           err)))))
  (advice-add 'org-babel-execute-src-block :before 'my/org-babel-execute-src-block)
  (setq org-confirm-babel-evaluate nil)
  (message "[USE-PACKAGE:config] - ob-core"))

;; ---------------------------------------------------
;;
;;;; configs
;;
;; ---------------------------------------------------
(use-package org-crypt
  :straight (:type built-in)
  :after org
  :custom
  (org-tags-exclude-from-inheritance (quote ("crypt")))
  (org-crypt-disable-auto-save nil)
  :config
  (org-crypt-use-before-save-magic))

(use-package org-refile
  :straight (:type built-in)
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

  (setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 4))))


;; ---------------------------------------------------
;;
;;;; add-ons
;;
;; ---------------------------------------------------

;; https://github.com/nobiot/org-transclusion
(use-package org-transclusion
  :straight t
  :after org
  ;;:bind ("C-c t" . org-transclusion-add)
  )


(provide 'mode/extended/org-extensions)
