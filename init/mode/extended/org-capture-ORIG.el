;;; -*- lexical-binding: t; -*-

;; ~/bin/emacs-org-protocol-capture names the new frame "org-protocol-capture"

(eval-after-load 'org-capture 
  (progn
    (message "EVAL AFTER CAPTURE")

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (message "DEFADVICE org-capture-finalize"))

    
  (defadvice org-capture
      (after make-full-window-frame activate)
    "Advise capture to be the only window when used as a popup"
    (message "Org capture frame %s" (frame-parameter nil 'name))
    (if (equal "org-protocol-capture" (frame-parameter nil 'name))
        (delete-other-windows)))

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (when (and (equal "org-protocol-capture" (frame-parameter nil 'name))
               (not (eq this-command 'org-capture-refile)))
      (delete-frame)))

  (defadvice org-capture-destroy
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (when (and (equal "org-protocol-capture" (frame-parameter nil 'name))
               (not (eq this-command 'org-capture-refile)))
      (delete-frame)))

  (defadvice org-capture-refile
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (when (equal "org-protocol-capture" (frame-parameter nil 'name))
      (delete-frame)))

  ;; Is this a good idea?
  (defadvice org-capture-refile (after save-after-refile-advice activate)
    (org-save-all-org-buffers))


  ;; make the frame contain a single window. by default org-capture
  ;; splits the window.
  (add-hook 'org-capture-mode-hook
            'delete-other-windows)

 
))


;;(defun stag-misanthropic-capture (&rest r)
;;  (message "AFTER org-capture-place-template")
;;  (delete-other-windows))

;;(advice-add  #'org-capture-place-template :after 'stag-misanthropic-capture)

;;;  TODO try
;; (eval-after-load 'org
;;   (progn 
;;     (defun +org-fix-delete-other-windows-a (orig-fn &rest args)
;;       "docstring"
;;       (interactive "P")
;;       (if popper-mode 
;;           (cl-letf (((symbol-function #'delete-other-windows)
;;                      (symbol-function #'ignore))
;;                     ((symbol-function #'delete-window)
;;                      (symbol-function #'ignore)))
;;             (apply orig-fn args))
;;         (apply orig-fn args)))

;;     (dolist (org-function '(org-add-log-note
;;                             org-capture-place-template
;;                             org-export--dispatch-ui
;;                             org-agenda-get-restriction-and-command
;;                             org-fast-tag-selection
;;                             org-fast-todo-selection))

;;       (advice-add org-function :around #'+org-fix-delete-other-windows-a))))

(provide 'mode/extended/org-capture)
