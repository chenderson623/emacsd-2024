;;; -*- lexical-binding: t; -*-

(defvar my$org-protocol-capture-frame-name "*Org Capture*")

(defun org-capture-frame/delete-other-windows-if-capture-frame (&rest args)
  "Delete the extra window if we are in a capture frame"
  (when (equal my$org-protocol-capture-frame-name (frame-parameter nil 'name))
    (delete-other-windows)))

(defun org-capture-frame/delete-capture-frame-if-not-refile (&rest args)
  "Advise capture-finalize to close the frame"
  (when (and (equal my$org-protocol-capture-frame-name (frame-parameter nil 'name))
             (not (eq this-command 'org-capture-refile)))
    (delete-frame)))

(defun org-capture-frame/delete-capture-frame (&rest args)
  "Advise capture-finalize to close the frame"
  (when (equal my$org-protocol-capture-frame-name (frame-parameter nil 'name))
    (delete-frame)))

(defun org-capture-frame>make-capture-frame (&optional capture-url)
  "Create a new frame and run org-capture. Call with emacsclient -ne '(make-capture-frame)'"
  (interactive)
  (make-frame-on-display ":0" `((name . ,my$org-protocol-capture-frame-name)
                                (width . 120)
                                (height . 30)))
  (select-frame-by-name my$org-protocol-capture-frame-name)
  (condition-case err
      (if capture-url (org-protocol-capture capture-url) (org-capture))
    (error (message (format "Caught exception: [%s]" err))
           (when (equal my$org-protocol-capture-frame-name (frame-parameter nil 'name))
             ;; Delete the frame if there was an error, which is the case in particular
             ;; if you pressed "q" in the template selection.
                                        ;(delete-frame)
             ;; This is needed to stop listening for keystrokes in the main window.
                                        ;(keyboard-quit)
             ))))

(defun org-capture-frame>make-capture-html-frame (&optional capture-url)
  "Create a new frame and run org-capture. Call with emacsclient -ne '(make-capture-frame)'"
  (interactive)
  (make-frame-on-display ":0" `((name . ,my$org-protocol-capture-frame-name)
                                (width . 120)
                                (height . 30)))
  (select-frame-by-name my$org-protocol-capture-frame-name)
  (condition-case err
      (if capture-url (org-protocol-capture-html--with-pandoc capture-url) (org-capture))
    (error (message (format "Caught exception: [%s]" err))
           (when (equal my$org-protocol-capture-frame-name (frame-parameter nil 'name))
             ;; Delete the frame if there was an error, which is the case in particular
             ;; if you pressed "q" in the template selection.
                                        ;(delete-frame)
             ;; This is needed to stop listening for keystrokes in the main window.
                                        ;(keyboard-quit)
             ))))

(defun org-capture-frame/setup-org-protocol-capture-frame ()  
  (advice-add 'org-capture-place-template :after #'org-capture-frame/delete-other-windows-if-capture-frame)
  (advice-add 'switch-to-buffer-other-window :after #'org-capture-frame/delete-other-windows-if-capture-frame)
  
  (advice-add 'org-capture-finalize :after #'org-capture-frame/delete-capture-frame-if-not-refile)
  (advice-add 'org-capture-destroy :after #'org-capture-frame/delete-capture-frame-if-not-refile)
  
  (advice-add 'org-capture-refile :after #'org-capture-frame/delete-capture-frame)

  (add-to-list 'org-protocol-protocol-alist
               '("org-capture" :protocol "capture"
                 :function org-capture-frame>make-capture-frame
                 :kill-client t))

  (add-to-list 'org-protocol-protocol-alist
               '("capture-html" :protocol "capture-html"
                 :function org-capture-frame>make-capture-html-frame
                 :kill-client t)))

(with-eval-after-load 'org-protocol
  (org-capture-frame/setup-org-protocol-capture-frame))

(provide 'mode/extended/org-capture-frame)
