;;; -*- lexical-binding: t; -*-

(require 'dash)

;; adopted from: https://stackoverflow.com/questions/7509463/how-to-move-a-subtree-to-another-subtree-in-org-mode-emacs

(defvar org-refile-immediate nil
  "Refile immediately using `org-refile-immediate-target' instead of prompting.")
(make-local-variable 'org-refile-immediate)

(defvar org-refile-immediate-preserve-order t
  "If last command was also `org-refile' then preserve ordering.")
(make-local-variable 'org-refile-immediate-preserve-order)

(defvar org-refile-immediate-target nil)
"Value uses the same format as an item in `org-refile-targets'."
(make-local-variable 'org-refile-immediate-target)

(defadvice org-refile (around org-immediate activate)
  (if (not org-refile-immediate)
      ad-do-it
    ;; if last command was `org-refile' then preserve ordering
    (let ((org-reverse-note-order
           (if (and org-refile-immediate-preserve-order
                    (eq last-command 'org-refile)) nil org-reverse-note-order)))
      (ad-set-arg 2 (assoc org-refile-immediate-target (org-refile-get-targets)))
      (prog1 ad-do-it
        (setq this-command 'org-refile)))))

(defadvice org-refile-cache-clear (after org-refile-history-clear activate)
  (setq org-refile-targets (default-value 'org-refile-targets))
  (setq org-refile-immediate nil)
  (setq org-refile-immediate-target nil)
  (setq org-refile-history nil))

;;;###autoload
(defun org-refile-immediate-target (&optional arg)
  "Set current entry as `org-refile' target.
Non-nil turns off `org-refile-immediate', otherwise `org-refile'
will immediately refile without prompting for target using most
recent entry in `org-refile-targets' that matches
`org-refile-immediate-target' as the default."
  (interactive "P")
  (if (equal arg '(16))
      (progn
        (setq org-refile-immediate-preserve-order
              (not org-refile-immediate-preserve-order))
        (message "Order preserving is turned: %s"
                 (if org-refile-immediate-preserve-order
                     "on" "off")))

    (setq org-refile-immediate (unless arg t))
    (make-local-variable 'org-refile-targets)
    (let* ((components (org-heading-components))
           (level (first components))
           (heading (nth 4 components))
           (string (substring-no-properties heading)))
      (add-to-list 'org-refile-targets
                   (append (list (buffer-file-name))
                           (cons :regexp
                                 (format "^%s %s$"
                                         (make-string level ?*)
                                         string))))
      (setq org-refile-immediate-target heading))))

;;(define-key org-mode-map "\C-c\C-x\C-m" 'org-refile-immediate-target)

(provide 'org-refile-immediate)
;;; org-refile-immediate.el ends here
