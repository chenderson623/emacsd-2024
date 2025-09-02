;;;-*- lexical-binding: t; -*-

(eval-when-compile
  (require 'mode/prog/treesit)
  (require 'mode/prog/flycheck)
  (require 'mode/prog/eglot)
  (require 'mode/prog/lsp-mode)
  (require 'pretty-hydra)
  )

;;
;;; Define some defaults
;;
(defvar php$ide-level "lsp-mode") ;; ac-php, phpactor, lsp-mode, eglot

(defvar php$phpactor-install-directory "/home/chris/.local/share/php-global")
(defvar php$phpactor-executable-filepath "/home/chris/.local/share/php-global/vendor/bin/phpactor")

(setq lsp-phpactor-path
      "/home/chris/.local/share/php-global/vendor/bin/phpactor"
       )

(setq flycheck-phpcs-standard "PSR2"
        ;;flycheck-php-executable "/opt/local/bin/php"
        ;;flycheck-php-phpcs-executable "~/.composer/vendor/bin/phpcs"
        ;;flycheck-php-phpmd-executable "~/.composer/vendor/bin/phpmd"
        )

(setq flycheck-phpcs-standard "~/.config/phpcs/phpcs.xml")

;;
;;; Init hook for php files
;;
(defun php:php-mode-init ()
  (message "PHP_MODE_HOOK")

  ;; style
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (setq-local c-basic-offset 4)
  (turn-on-auto-fill)

  (electric-indent-mode)
  (electric-pair-mode)
  (electric-layout-mode)

  (subword-mode 1)

  (company-mode +1)

  ;; lsp
  (setq-local lsp-enable-imenu nil)

  (cond
   ((equal php$ide-level "ac-php")
    (php:ac-php-init))
   ((equal php$ide-level "phpactor")
    (php:phpactor-init))
   ((equal php$ide-level "eglot")
    (php:eglot-init))
   ((equal php$ide-level "lsp-mode")
    (php:lsp-mode-init))
   ))

;;
;;; Keyboard prefixes
;;
(define-prefix-command 'composer-prefix-map)

(defvar-keymap my-php-mode-map
  :doc "Keymap for major-mode leader"
  ;;:keymap mode-specific-map
  "c" (cons "composer" composer-prefix-map)
  ;;"C" 'composer-hydra/body
  ;;"t" 'cat-toggle-prefix
  )

(with-eval-after-load 'php-ts-mode
  (define-key php-ts-mode-map (kbd "C-c ,") (cons "My PHP Mode Map" my-php-mode-map))
  )

;;
;;; php-mode
;;
(use-package php-mode
  :straight t
  ;;:hook (php-mode . php:php-mode-init)
  )

;;
;;; php-ts-mode
;;
;; setup:
;; (php-ts-mode-install-parsers)
(use-package php-ts-mode
  :straight (:type built-in)
  ;;:mode "\\.php\\'"
  ;;:hook (php-ts-mode . php:php-mode-init)
  :config
  (require 'treesit)
  
  (add-to-list 'major-mode-remap-alist '(php-mode . php-ts-mode))
  (add-to-list 'major-mode-remap-alist '(php-mode-maybe . php-ts-mode))

  (setq treesit-font-lock-level 4) ;; Maximum treesit font decoration

  ;;(define-key php-ts-mode-map (kbd "C-c c") (cons "Composer" 'composer-prefix-map))
  )

;;
;;; --------------------- ac-php ---------------------
;; 

;; https://github.com/xcwen/ac-php
(use-package company-php
  :straight t
  :commands (php:ac-php-init)
  :config
  (setq ac-php-tags-path (emacs-cache*filepath "ac-php"))

  ;;
  ;;;; init for ac-php
  ;;
  (defun php:ac-php-init ()

    ;; Enable ElDoc support (optional)
    (ac-php-core-eldoc-setup)
    (eldoc-mode +1)

    ;; setup company
    (company-mode +1)
    (set (make-local-variable 'company-backends)
         '((company-ac-php-backend company-dabbrev-code)
           company-capf company-files)))

  (bind-keys :package company-php :map php-ts-mode-map
             ("C-M-]" . ac-php-find-symbol-at-point)
             ("C-M-[" . ac-php-location-stack-back)))

;;
;;; --------------------- phpactor ---------------------
;;

;; https://github.com/emacs-php/phpactor.el
;; after installing this package, run `phpactor-install-or-update`
(use-package phpactor
  :straight t
  :commands (php:phpactor-init)
  :init 
  (setq phpactor-install-directory php$phpactor-install-directory)
  (setq phpactor-executable php$phpactor-executable-filepath)
  :config
  (use-package company-phpactor
    :straight t)

  (use-package my-php-mode/php/phpactor-transient-menu
    :straight nil
    :bind (:map my-php-mode-map
                ("m" . phpactor-transient-menu)
                )  
    )

  ;;
  ;;;; init for ac-php
  ;;
  (defun php:phpactor-init ()
    (require 'php-mode)
  
    ;; completion
    (company-mode +1)
    (require 'company-phpactor)

    (set (make-local-variable 'company-backends)
         '(;; list of backends
           company-phpactor
           company-capf
           company-files
           ))
  
    ;; eldoc
    ;; laggy?
    ;; TODO change eldoc to not be on hover? key binding?
    (make-local-variable 'eldoc-documentation-function)
    (setq eldoc-documentation-function
          'phpactor-hover)
    (eldoc-mode nil)

    ;; smart jump
    (phpactor-smart-jump-register)
    )
  
  ;; (bind-keys :package phpactor :map php-ts-mode-map
  ;;            ("M-." . phpactor-goto-definition)
  ;;            ("M-?" . phpactor-find-references))

  (use-package smart-jump
    :straight t
    :after xref
    )

  ;; (bind-keys :package smart-jump :map php-ts-mode-map
  ;;            ;; ("M-." . smart-jump-go)
  ;;            ;; ("M-," . smart-jump-back)
  ;;            ;; ("M-?" . smart-jump-references)
  ;;            )

  )

;;
;;; --------------------- eglot ---------------------
;;

(use-package eglot
  :straight (:type built-in)
  :commands (php:eglot-init)
  :config
  (message "USE-PACKAGE::config: eglot local")

  (add-to-list 'eglot-server-programs '((php-mode phps-mode php-ts-mode) "phpactor" "language-server"))

  ;;
  ;;;; init for php eglot
  ;;
  (defun php:eglot-init ()
    (eglot-ensure)
    )
  )

;;
;;; --------------------- lsp-mode ---------------------
;;
(use-package lsp-mode
  :straight t
  :commands (php:lsp-mode-init)
  :config
  ; TODO are these already lists?
  (setq lsp-enabled-clients '(phpactor))
  (setq lsp-disabled-clients '(php-ls iph intelephense serenata))
  
  (defun php-local-checkers()
    (setq-local flycheck-local-checkers-chain '((lsp . phpstan)
                                                (phpstan . php-phpmd))))
  (defun php-disable-lsp-imenu()
    "The imenu produced by php-ts-mode is better than what lsp intelephense outputs"
    (setq-local lsp-enable-imenu nil))

  (add-hook 'php-ts-mode-hook 'php-local-checkers)
  (add-hook 'php-ts-mode-hook 'php-disable-lsp-imenu)
  (add-hook 'php-ts-mode-hook (lambda() (setq-local format-all-formatters '(("PHP" (prettier "--brace-style=1tbs"))))))

  ;;
  ;;;; init for php eglot
  ;;
  (defun php:lsp-mode-init ()
    (lsp)
    )  
  )

;;
;;; Linting
;;

;;;; phpstan
;; https://github.com/emacs-php/phpstan.el
(use-package flycheck-phpstan
  :straight t
  :after (flycheck)
  :config
  (setq-default phpstan-level 8)
  (setq-default phpstan-memory-limit "4G")
  )

;;
;;; PHP Packages
;;

;;;; composer
;; https://github.com/emacs-php/composer.el
(use-package composer
  :straight t
  :bind (
         :map composer-prefix-map
         ("c" . composer)
         ("g" . php:composer>global)
         ("d" . php:composer>require-dev)
         ("i" . composer-install)
         ("j" . composer-find-json-file)
         ("l" . composer-find-lock-file)
         ("p" . composer-dump-autoload)
         ("r" . composer-require)
         ("s" . composer-run-script)
         ("t" . composer-list-packages)
         ("u" . composer-update)
         ("v" . composer-run-vendor-bin-command)
         )
  :config
  (defun php:composer>global ()
    "Run `composer' (global) sub command (with completing read)"
    (interactive)
    (let ((current-prefix-arg '(4)))    ; Sets the prefix argument to C-u
      (call-interactively #'composer)))
  (defun php:composer>require-dev ()
    "Run 'composer require --dev' command"
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'composer-require)))

  (pretty-hydra-define composer-hydra
                       (:color amaranth :quit-key "q" :title "PHP Composer")
                       (
                        "Command"
                        (
                         ("c" composer)
                         ("i" composer-install)
                         ("p" composer-dump-autoload)
                         ("t" composer-list-packages)
                         ("u" composer-update)
                         )
                        "Require"
                        (
                         ("r" composer-require "require")
                         ("g" php:composer>global "require - global")
                         ("d" php:composer>require-dev "require - dev")
                         )
                        "Run"
                        (
                         ("s" composer-run-script)
                         )
                        "Files"
                        (
                         ("j" composer-find-json-file)
                         ("l" composer-find-lock-file)
                         )
                        ))
  )

;;;; phpunit
;; https://github.com/nlamirault/phpunit.el
(use-package phpunit
  :straight t
  :after (php-mode php-ts-mode)
  :bind (:map php-ts-mode-map
              ("C-t c" . phpunit-current-class)
              ("C-t t" . phpunit-current-test)))

;; https://github.com/cfclrk/php-cs-fixer-format
;; uses: https://github.com/PHP-CS-Fixer/PHP-CS-Fixer
;; uses: https://github.com/purcell/emacs-reformatter
(use-package php-cs-fixer-format
  :after (php-mode splash)
  :straight (php-cs-fixer-format
             :host github
             :depth nil
             :repo "cfclrk/php-cs-fixer-format")
  :config
  (setq php-cs-fixer-format-arguments (list "--config"
                                            (concat splash-website-dir "/.php-cs-fixer.php"))))

;; https://github.com/emacs-php/psysh.el
(use-package psysh 
  :straight t
  :commands psysh)

(use-package dape
  :straight t
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  (setq dape-key-prefix "\C-x\C-a")

  ;;:hook
  ;; Save breakpoints on quit
  ;; (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  ;; (after-init . dape-breakpoint-load)

  ;;:config
  ;; Turn on global bindings for setting breakpoints with mouse
  ;; (dape-breakpoint-global-mode)

  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
  ;; (setq dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
  ;; (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Projectile users
  ;; (setq dape-cwd-function 'projectile-project-root)
  )

;; TODO where is this?
(use-package php-doc-block
  :straight nil
  :commands (php-doc-block))

(provide 'mode/language/php)
