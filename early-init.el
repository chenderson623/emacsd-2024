;;; early-init.el --- early init file  -*- lexical-binding: t; -*-

;; Measure startup time
(defconst emacs-start-time (current-time))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;
;;;; Native compilation settings
;;
(when (featurep 'native-compile)
  ;; Silence native comp warnings popping up 
  (customize-set-variable 'native-comp-async-report-warnings-errors nil)
  ;; Optimization level for native compilation, a number between -1 and 3
  (customize-set-variable 'native-comp-speed -1)
  ;; Silence compiler warnings as they can be pretty disruptive
  (customize-set-variable 'native-comp-deferred-compilation t)

  ;; Set the right directory to store the native compilation cache
  (setq my$emacs-eln-cache-dir (convert-standard-filename "~/.cache/emacs/eln-cache"))
  (startup-redirect-eln-cache my$emacs-eln-cache-dir)

  (add-to-list 'native-comp-eln-load-path my$emacs-eln-cache-dir))

;;;; Prefer Newer files
;; Prefer newer versions of files
(setq load-prefer-newer t)

;;
;;;; Set up garbage collection
;;
;; Make startup faster by reducing the frequency of garbage collection
;;     gc-cons-threshold default is 0.8MB.
;;     gc-cons-percentage is portion of heap used for allocation. Defaults to 0.1.
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook `(lambda ()
                              (setq gc-cons-threshold 800000
                                    gc-cons-percentage 0.1)
                              (garbage-collect)) t)

;;
;;;; Early package and file handling settings
;;

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq-default package-enable-at-startup nil)   ;; using straight
(advice-add #'package--ensure-init-file :override #'ignore)

;; Unset `file-name-handler-alist' too (temporarily). Every file opened and
;; loaded by Emacs will run through this list to check for a proper handler for
;; the file, but during startup, it won’t need any of them.
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old)))

;; If an `.el' file is newer than its corresponding `.elc', load the `.el'.
(setq load-prefer-newer t)

;;
;;;; Early UI settings
;;

;; Faster to disable these here (before they've been initialized)
;;(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Give the frame basic coloring while waiting for the theme to load.
;; Avoid flashing a screen full of white.
;;;(set-face-attribute 'default nil :background "#282c34" :foreground "#bbc2cf")

;; Default frame settings. This is actually maximized, not full screen.
;;(push '(fullscreen . maximized) initial-frame-alist)
;;(push '(ns-transparent-titlebar . t) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

(setq inhibit-startup-echo-area-message 't
      inhibit-startup-message 't
      initial-major-mode 'fundamental-mode
      initial-scratch-message 'nil)

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
