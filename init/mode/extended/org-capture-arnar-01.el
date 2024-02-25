;;; -*- lexical-binding: t; -*-

(defvar my$org-protocol-capture-frame-name "*Org Capture*")

;; from: https://github.com/arnar/dotfiles
(defun my:setup-org-protocol-capture-frame ()
  (require 'org-protocol)
  (require 'org-capture)
    
  (defadvice org-switch-to-buffer-other-window
      (after suppress-window-splitting activate)
    "Delete the extra window if we are in a capture frame"
    (if (equal "*Org Capture*" (frame-parameter nil 'name))
        (delete-other-windows)))
          
  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "*Org Capture*" (frame-parameter nil 'name))
        (delete-frame)))
          
  (defun make-capture-frame (&optional capture-url)
    "Create a new frame and run org-capture. Call with emacsclient -ne '(make-capture-frame)'"
    (interactive)
    (make-frame-on-display ":0" '((name . "*Org Capture*")
                                  (width . 120)
                                  (height . 30)))
    (select-frame-by-name "*Org Capture*")
    (condition-case err
        (if capture-url (org-protocol-capture capture-url) (org-capture))
      (error (message (format "Caught exception: [%s]" err))
             (when (equal "*Org Capture*" (frame-parameter nil 'name))
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
  )

(add-hook 'emacs-startup-hook #'my:setup-org-protocol-capture-frame 101)

(provide 'mode/extended/org-capture-arnar-01)
