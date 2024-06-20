;;; -*- lexical-binding: t; -*-

;; maybe like this:
;; (use-package ob
;;   :config
;;   (setq org-plantuml-jar-path (expand-file-name "/usr/share/java/plantuml/plantuml.jar"))
;;   (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
;;   (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;;   (add-to-list 'org-src-lang-modes '("awk" . awk))
;;   (add-to-list 'org-src-lang-modes '("ditaa" . plantuml))

;;   (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)
;;                                                            (ditaa .t)
;;                                                            (awk .t))))

;; add to list
;; (use-package ob-js
;;   :config
;;   (add-to-list 'org-babel-load-languages '(js . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;;   (add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))
;;   (system-packages-ensure "node"))
;;   ;; use “:results output” for js blocks!

;; clean
    ;; (use-package ob-restclient
    ;;   :ensure t
    ;;   :config
    ;;   (org-babel-do-load-languages 'org-babel-load-languages
    ;;                                (append org-babel-load-languages
    ;;                                        '((restclient . t)))))

    ;; (use-package ob-http
    ;;   :ensure t
    ;;   :config
    ;;   (org-babel-do-load-languages 'org-babel-load-languages
    ;;                                (append org-babel-load-languages
    ;;                                        '((http . t)))))

;; lazy-load?
  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((emacs-lisp . t)
           (shell . t)
           (sql . t)
           (org . t))))

  (defadvice org-babel-execute-src-block (around load-language nil activate)
    "Load language if needed"
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      ad-do-it))
;; 


;;;  Lazy load languages
(use-package ob-core
  :after org
  :config
  (defun my/org-babel-execute-src-block (&optional _arg info _params)
    "Load language if needed"
    (let* ((lang (nth 0 info))
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
  (setq org-confirm-babel-evaluate nil))


(provide 'mode/extended/org-ob)
