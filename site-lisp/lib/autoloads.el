;;; autoloads.el  -*- lexical-binding: t; -*-
;; 
;;; Code:

(eval-and-compile
  (require 'loaddefs-gen))

;;---------------------------------------------------------------------
;; Autoload Functions
;;---------------------------------------------------------------------
(defun autoloads:define (path-list autoloads-filepath)
  ;; if loaddefs does not exist, load defuns and build
  (when (not (file-exists-p autoloads-filepath))
    (message (format "Generate %S" autoloads-filepath))
    (autoload:generate-loaddefs-file autoloads-filepath path-list))
  (load autoloads-filepath nil t))

(defun autoload:generate-loaddefs-file (loaddef-file-path autoload-dirs)
  (loaddefs-generate autoload-dirs loaddef-file-path)
  (byte-compile-file loaddef-file-path))

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
