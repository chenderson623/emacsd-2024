;;; -*- lexical-binding: t; -*-

(defvar my$org-protocol-capture-frame-name "*Org Capture*")

;; from: https://github.com/arnar/dotfiles
(defun my:setup-org-protocol-capture-frame ()
  (require 'org-protocol)
  (require 'org-capture)
  
  ;; 2024-02-29 this was changed from org-switch-to-buffer-other-window (obsolete function)
  (defadvice switch-to-buffer-other-window
      (after suppress-window-splitting activate)
    "Delete the extra window if we are in a capture frame"
    (if (equal my$org-protocol-capture-frame-name (frame-parameter nil 'name))
        (delete-other-windows)))

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (when (and (equal my$org-protocol-capture-frame-name (frame-parameter nil 'name))
               (not (eq this-command 'org-capture-refile)))
      (delete-frame)))

  (defadvice org-capture-destroy
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (when (and (equal my$org-protocol-capture-frame-name (frame-parameter nil 'name))
               (not (eq this-command 'org-capture-refile)))
      (delete-frame)))

  (defadvice org-capture-refile
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (when (equal my$org-protocol-capture-frame-name (frame-parameter nil 'name))
      (delete-frame)))
  
  (defun make-capture-frame (&optional capture-url)
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

    (defun make-capture-html-frame (&optional capture-url)
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

  (add-to-list 'org-protocol-protocol-alist
               '("org-capture" :protocol "capture"
                 :function make-capture-frame
                 :kill-client t))

  (add-to-list 'org-protocol-protocol-alist
               '("capture-html" :protocol "capture-html"
                 :function make-capture-html-frame
                 :kill-client t))
  )

(add-hook 'emacs-startup-hook #'my:setup-org-protocol-capture-frame 101)

(provide 'mode/extended/org-capture-frame)
