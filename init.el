;;; init.el --- Emacs init file -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Author: Chris Henderson <chenderson623@gmail.com>
;; Created: 23 December 2023
;; Keywords: emacs init
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;; Base init file to load config. Use "outline" (<Tab> and <S-Tab>
;; in org style) to navigate through sections, and "imenu"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(defvar emacsd$dir 
  (file-name-directory
   (file-chase-links load-file-name))
  "Directory that init.el is in.")

;;
;;;;; Set load paths
;;
(add-to-list 'load-path (expand-file-name "init/" emacsd$dir))
(add-to-list 'load-path (expand-file-name "site-lisp/" emacsd$dir))

;;;;; Define init hooks
(defvar emacsd-init@defvars-loaded-hook nil
 "Hook called after defvars are loaded.")
(defvar emacsd-init@package-manager-ready-hook nil
 "Hook called after use-package is ready.")
(defvar emacsd-init@modules-loaded-hook nil
 "Hook called after modules are loaded.")

;;
;;;;; Load local init file early
;;
(defvar emacsd-local$init-filename "init.local.el")
(defvar emacsd-local$config-dir
  (if (file-exists-p (expand-file-name emacsd-local$init-filename "~/.config/emacs"))
      "~/.config/emacs"
    emacsd$dir))

(when (file-exists-p (expand-file-name emacsd-local$init-filename emacsd-local$config-dir))
  (message "Load local emacsd")
  (load-file (expand-file-name emacsd-local$init-filename emacsd-local$config-dir)))

;;
;;;;; Set up custom.el
(setq custom-file (expand-file-name "custom.el" emacsd-local$config-dir))
(when (not (file-readable-p custom-file))
  (copy-file (expand-file-name "default-custom.el" emacsd$dir) custom-file))

;; load custom.el
(when (file-exists-p custom-file)
  (load custom-file))

;;
;;;;; Setup filepaths
;;
(require 'lib/filepaths)
;; XDG specification and standard support
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/xdg.el
(require 'xdg)

(message "Set up functionalized filepaths")
;; create function user-home*filepath
(filepath%functionalize-path user-home (xdg--dir-home "HOME" "~"))
;; create function user-state*filepath
(filepath%functionalize-path user-state (xdg-state-home))
;; create function user-shared*filepath
(filepath%functionalize-path user-shared (xdg-data-home))
;; create function user-cache*filepath
(filepath%functionalize-path user-cache (xdg-cache-home))
;; create function user-config*filepath
(filepath%functionalize-path user-config (xdg-config-home))

;; create function emacsd*filepath
(filepath%functionalize-path emacsd emacsd$dir)
;; create function emacsd-local-config*filepath
(filepath%functionalize-path emacsd-local-config emacsd-local$config-dir)
;; create function emacs-shared*filepath, const filepath$$emacs-shared
(filepath%functionalize-path emacs-shared (user-shared*filepath "emacs") t)
;; create function emacs-state*filepath, const filepath$$emacs-state
(filepath%functionalize-path emacs-state (user-state*filepath "emacs") t)
;; create function emacs-cache*filepath
(filepath%functionalize-path emacs-cache (user-cache*filepath "emacs") t)
;; create function emacs-config*filepath
(filepath%functionalize-path emacs-config (user-config*filepath "emacs") t)

(defun emacsd>open-init.el ()
    (interactive)
  (find-file (emacsd*filepath "init.el")))

;;
;;;; System Variables
;;
;; Check the system used
(defconst sys-linux   (eq system-type 'gnu/linux))
(defconst sys-mac     (eq system-type 'darwin))
(defconst sys-bsd     (or sys-mac (eq system-type 'berkeley-unix)))
(defconst sys-win     (memq system-type '(cygwin windows-nt ms-dos)))

(run-hooks 'emacsd-init@defvars-loaded-hook)

;;
;;;; Autoloads
;;
(require 'lib/autoloads)
(autoloads:define
   (list
    (emacsd*filepath "site-lisp/action"))
   (emacsd*filepath "site-lisp/autoloads-action.el"))
(autoloads:define
   (list
    (emacsd*filepath "site-lisp/contrib"))
   (emacsd*filepath "site-lisp/autoloads-contrib.el"))

;;
;;;; Setup package manager
;;
(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/")))

;;
;;;;; Setup straight
;;
(setq straight-base-dir filepath$$emacs-shared)  ; set base directory for straight

;;;;; Bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;; Load use-package
;; Good documentation on use-package keywords: https://jwiegley.github.io/use-package/keywords/
(straight-use-package 'use-package)
(use-package straight
  :custom
  (straight-use-package-by-default nil)) ;; use straight by default

;; Use-Package Settings
(use-package use-package
  :custom
  ;; Always defer
  (use-package-always-defer t)
  ;; Report loading details
  (use-package-verbose nil)
  ;; This is really helpful for profiling
  (use-package-minimum-reported-time 0)
  ;; Expand normally
  (use-package-expand-minimally nil)
  ;; Unless otherwise set, manually handle package install
  ;;(use-package-always-ensure nil)
  ;; Navigate use-package declarations w/imenu
  (use-package-enable-imenu-support t))

;;(straight-use-package '(once :type git :host github :repo "emacs-magus/once"))
;;(straight-use-package 'setup)

(add-hook 'after-init-hook
	  (lambda ()
	    (message "AFTER INIT")
))

(run-hooks 'emacsd-init@package-manager-ready-hook)

;; (require 'emacs/core-settings)
;; (require 'emacs/core-keybindings)
;; (require 'emacs/extensions)
;; (require 'feature/search)

;;;;; Setup modular config
;; https://github.com/SidharthArya/modular-config.el
;; emacs --config none --modules "core appearance vi"
(use-package modular-config
  :straight t
  :demand t
  :custom
  (modular-config-list '((none ())
                         (core (
                                emacs/core-settings
				                emacs/core-keybindings
				                emacs/extensions
                                ))
                         (full ((core)
				                emacs/shell
                                emacs/server
				                ui/manage-themes
                                ;;ui/modus-themes
				                ui/manage-fonts
				                ui/pulsar
                                ui/keybindings
				                mode/dired
                                mode/ibuffer
				                mode/filetype-modes
				                mode/org
                                mode/extended/org-capture-frame
                                ;;mode/extended/org-folding
                                mode/extended/org-ob
				                mode/prog
                                mode/eshell
                                mode/treesit
				                feature/completion-at-point
                                feature/completing-read
				                feature/discoverability
                                feature/editing
                                feature/files
				                feature/minibuffer
                                feature/navigation
				                feature/notetaking
                                feature/outline
				                feature/project
				                feature/search
				                feature/spelling
				                feature/templates
				                feature/vcs
				                feature/web
				                feature/windows
                                ))
                         (testing ((full)
                                   ))
                         ))

  ;; default group to load
  (modular-config-default 'full)
  (modular-config-path (emacsd*filepath "init"))
  (modular-config-use-separate-bookmarks nil)
  :config
  (modular-config-command-line-args-process))

(run-hooks 'emacsd-init@modules-loaded-hook)

(provide 'init)
;;; init.el ends here
