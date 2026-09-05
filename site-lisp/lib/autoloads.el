;;; autoloads.el  -*- lexical-binding: t; -*-
;; 
;;; Code:

(eval-and-compile
  (require 'loaddefs-gen))

;;---------------------------------------------------------------------
;; Autoload Functions
;;---------------------------------------------------------------------
(defvar autoloads--registrations nil)
(defvar autoloads--pending-registrations nil)
(defvar autoloads--refresh-timer nil)

(defun autoloads--refresh-registration (registration)
  (let ((autoloads-file (cdr registration)))
    (condition-case err
        (progn
          (message "Refresh autoloads: %s" autoloads-file)
          (autoload:generate-loaddefs-file autoloads-file (car registration))
          (load autoloads-file nil 'nomessage))
      (error
       (message "Could not refresh autoloads for %s: %s"
                autoloads-file (error-message-string err))))))

(defun autoloads--refresh-pending ()
  (setq autoloads--refresh-timer nil)
  (let ((registrations autoloads--pending-registrations))
    (setq autoloads--pending-registrations nil)
    (dolist (registration registrations)
      (autoloads--refresh-registration registration))))

(defun autoloads--after-save ()
  (when buffer-file-name
    (dolist (registration autoloads--registrations)
      (when (and (not (equal (expand-file-name buffer-file-name)
                             (cdr registration)))
                 (seq-some (lambda (directory)
                             (file-in-directory-p buffer-file-name directory))
                           (car registration)))
        (unless (member registration autoloads--pending-registrations)
          (push registration autoloads--pending-registrations)))))
  (when (and autoloads--pending-registrations
             (not (timerp autoloads--refresh-timer)))
    (setq autoloads--refresh-timer
          (run-with-idle-timer 1 nil #'autoloads--refresh-pending))))

(defun autoloads:define (path-list autoloads-filepath)
  (let ((registration
         (cons (mapcar #'expand-file-name path-list)
               (expand-file-name autoloads-filepath))))
    (unless (member registration autoloads--registrations)
      (push registration autoloads--registrations)
      (add-hook 'after-save-hook #'autoloads--after-save)))
  (unless (file-exists-p autoloads-filepath)
    (message "Generate %S" autoloads-filepath)
    (autoload:generate-loaddefs-file autoloads-filepath path-list))
  (load autoloads-filepath nil t))

(defun autoload:generate-loaddefs-file (loaddef-file-path autoload-dirs)
  (loaddefs-generate autoload-dirs loaddef-file-path))

(defun autoload:regenerate-loaddefs-file (loaddef-file-path autoload-dirs)
  (when (file-exists-p loaddef-file-path)
    (delete-file loaddef-file-path t)
    (message "delete old autoload file: %s" loaddef-file-path))

  (autoload:generate-loaddefs-file loaddef-file-path autoload-dirs)
  (load loaddef-file-path nil 'nomessage)
  (message "reload core autoload file: %s done." loaddef-file-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autoloads.el ends here
(provide `lib/autoloads)
