;;; appearance -- Configured the visual appearance of emacs -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; CJH NOTES: This file is adopted from https://github.com/JonHarder/.emacs.d
;;    The theme definitions are exactly what I have been wanting to do
;;      1) be able to define themes without loading theme or defining theme path
;;      2) be able to save current theme to custom.el so different systems can have different themes
;;      3) switch between light and dark of the same theme family
;;    I removed much of the non-theme stuff and did a little refactoring. Added a custom variable for light/dark
;;; Code:

;;;; Define basic display configuration variables
(defgroup configuration nil
  "Group for personal configuration."
  :group 'emacs)

(defcustom manage-themes/theme-name "doom"
  "Theme to use, must be a key memeber of `manage-themes/theme-definitions'."
  :type 'string
  :group 'configuration)

(defcustom manage-themes/theme-mode "dark"
  "Theme mode: light/dark"
  :type 'string
  :group 'configuration)

(defvar manage-themes/theme-definitions (make-hash-table :test #'equal))

;; macro makes function 'make-theme
(cl-defstruct THEME_DEFINITION
  (package nil :type symbol)
  (light nil :type symbol)
  (dark nil :type symbol))

;; --------------------------------------------------------------------------
;; Define Themes
;; --------------------------------------------------------------------------

(puthash "terminal"
         (make-THEME_DEFINITION
          :package 'modus-themes
          :light 'modus-vivendi
          :dark 'modus-vivendi)
         manage-themes/theme-definitions)
(puthash "jetbrains"
         (make-THEME_DEFINITION
          :package 'jetbrains-darcula-theme
          :light 'jetbrains-darcula
          :dark 'jetbrains-darcula)
         manage-themes/theme-definitions)
(puthash "humanoid"
         (make-THEME_DEFINITION
          :package 'humanoid-themes
          :light 'humanoid-light
          :dark 'humanoid-dark)
         manage-themes/theme-definitions)
(puthash "acario"
         (make-THEME_DEFINITION
          :package 'doom-themes
          :light 'doom-acario-light
          :dark 'doom-acario-dark)
         manage-themes/theme-definitions)
(puthash "modus"
         (make-THEME_DEFINITION
          :package 'modus-themes
          :light 'modus-operandi
          :dark 'modus-vivendi)
         manage-themes/theme-definitions)
(puthash "solarized"
         (make-THEME_DEFINITION
          :package 'solarized-theme
          :light 'solarized-light
          :dark 'solarized-dark)
         manage-themes/theme-definitions)
(puthash "gruvbox"
         (make-THEME_DEFINITION
          :package 'doom-themes
          :light 'doom-gruvbox-light
          :dark 'doom-gruvbox)
         manage-themes/theme-definitions)
(puthash "doom"
         (make-THEME_DEFINITION
          :package 'doom-themes
          :light 'doom-one-light
          :dark 'doom-one)
         manage-themes/theme-definitions)
(puthash "oceanic"
         (make-THEME_DEFINITION
          :package 'doom-themes
          :dark 'doom-oceanic-next
          :light 'doom-tomorrow-day)
         manage-themes/theme-definitions)
(puthash "nord"
         (make-THEME_DEFINITION
          :package 'doom-themes
          :dark 'doom-nord
          :light 'doom-nord-light)
         manage-themes/theme-definitions)
(puthash "moonlight"
         (make-THEME_DEFINITION
          :package 'doom-themes
          :dark 'doom-moonlight
          :light 'doom-one-light)
         manage-themes/theme-definitions)
(puthash "tomorrow"
         (make-THEME_DEFINITION
          :package 'doom-themes
          :dark 'doom-tomorrow-night
          :light 'doom-tomorrow-day)
         manage-themes/theme-definitions)
(puthash "outrun-electric"
         (make-THEME_DEFINITION
          :package 'doom-themes
          :dark 'doom-outrun-electric
          :light 'doom-one-light)
         manage-themes/theme-definitions)
(puthash "material"
         (make-THEME_DEFINITION
          :package 'doom-themes
          :dark 'doom-material
          :light 'doom-one-light)
         manage-themes/theme-definitions)
(puthash "kaolin"
         (make-THEME_DEFINITION
          :package 'kaolin-themes
          :dark 'kaolin-temple
          :light 'kaolin-valley-light)
         manage-themes/theme-definitions)
(puthash "twilight"
         (make-THEME_DEFINITION
          :package 'twilight-anti-bright-theme
          :dark 'twilight-anti-bright
          :light 'twilight-anti-bright)
         manage-themes/theme-definitions)
(puthash "cyberpunk"
         (make-THEME_DEFINITION
          :package 'cyberpunk-theme
          :dark 'cyberpunk
          :light 'cyberpunk)
         manage-themes/theme-definitions)
(puthash "stimmung"
         (make-THEME_DEFINITION
          :package 'stimmung-themes
          :dark 'stimmung-themes-dark
          :light 'stimmung-themes-light)
         manage-themes/theme-definitions)
(puthash "shanty"
         (make-THEME_DEFINITION
          :package 'shanty-themes
          :dark 'shanty-themes-dark
          :light 'shanty-themes-light)
         manage-themes/theme-definitions)



;; --------------------------------------------------------------------------
;; Theme Customization
;; --------------------------------------------------------------------------

(defun jh/theme-customizations (selected-theme)
  "Perform any theme specific configuration for a given THEME."
  (cond
   ((string-prefix-p "modus-" (symbol-name selected-theme))
    (use-package modus-themes
      :custom
      (modus-themes-slanted-constructs t)
      (modus-themes-diffs 'bg-only)
      (modus-themes-mode-line '(borderless accented))
      (modus-themes-org-blocks 'tinted-background)
      (modus-themes-headings
      '((0 . (1.5))
        (1 . (1.3))
        (2 . (1.3))
        (3 . (1.2))
        (t . (rainbow semibold 1.1))))
      (modus-themes-markup '(bold italic intense))
      (modus-themes-common-palette-overrides
      '((fg-heading-1 blue-warmer)
        (fg-heading-2 yellow-cooler)
        (fg-heading-3 cyan-cooler)))
      (modus-themes-bold-constructs t)
      (modus-themes-syntax '(alt-syntax))
      (modus-themes-prompts '(intense background))
      (modus-themes-completions '((matches . (extrabold background intense))
                                  (selection . (semibold accented intense))
                                  (popup . (accented))))
      (modus-themes-paren-match '(bold underline intense))))

   ((string-prefix-p "humanoid-" (symbol-name selected-theme))
    (use-package humanoid-themes
      :custom
      (humanoid-themes-comment-bg t)
      (humanoid-themes-org-height nil)
      (humanoid-themes-org-agenda-hight t)
      (humanoid-themes-org-highlight t)))

   ((string-prefix-p "solarized-light-" (symbol-name selected-theme))
    (loadtheme/solarized-light))

   ((string-prefix-p "solarized-dark-" (symbol-name selected-theme))
    (loadtheme/solarized-dark))

   ;;; doom themes
   ((string-prefix-p "doom-" (symbol-name selected-theme))
    (use-package doom-themes
      :config
      ;; (doom-themes-org-config)
      :custom
      (doom-themes-enable-bold t)
      (doom-themes-enable-italic t)
      (doom-themes-padded-modeline nil)

      (doom-one-light-brighter-comments t)
      (doom-one-light-brighter-modeline t)

      (doom-one-brighter-comments t)

      (doom-tomorrow-night-padded-modeline t)
      (doom-one-brighter-comments t)
      (doom-one-brighter-modeline t)

      (doom-vibrant-brighter-comments t)
      (doom-vibrant-brighter-modeline t)

      (doom-outrun-electric-brighter-text t)

      (doom-oceanic-next-comment-bg t)
      (doom-oceanic-next-brighter-comments t)
      (doom-oceanic-next-brighter-modeline t)

      (doom-outrun-electric-brighter-modeline t)
      (doom-outrun-electric-brighter-comments t)))))

;; --------------------------------------------------------------------------
;; Internal functions
;; --------------------------------------------------------------------------

(defvar manage-themes$$current-theme nil)
(defvar manage-themes$$high-contrast-p nil)
(defvar manage-themes$$high-contrast-old-theme manage-themes/theme-name)

(defun manage-themes:set-theme (selected-theme)
  "Load THEME, disabling other enabled themes."
  (let ((other-themes (seq-filter (lambda (other-theme)
                                    (not (string-equal selected-theme other-theme)))
                                  custom-enabled-themes)))
    (unless (memq selected-theme custom-enabled-themes)
      (jh/theme-customizations selected-theme)
      (load-theme selected-theme t)
      (mapc 'disable-theme other-themes))))

(defun manage-themes:load-theme (theme package)
  "Load THEME, after installing PACKAGE if not found on system."
  (message (format "Loading theme %s" (symbol-name theme)))
  (unless (memq theme custom-known-themes)
    (straight-use-package package))
  (manage-themes:set-theme theme))

(defun reset-var (symbl)
  "Reset SYMBL to its standard value."e
  (set symbl (eval (car (get symbl 'standard-value)))))

;; --------------------------------------------------------------------------
;; Interactive functions
;; --------------------------------------------------------------------------

(defun manage-themes>select-theme (name)
  "Select the given theme, indexed by NAME. Uses the dark or light variant depending on system setting."
  (interactive (list (completing-read
                      "Theme: "
                      (hash-table-keys manage-themes/theme-definitions))))
  (setq manage-themes$$current-theme (gethash name manage-themes/theme-definitions)
        manage-themes/theme-name name)
  (let ((current-theme (if (string-equal manage-themes/theme-mode "dark")
                           (THEME_DEFINITION-dark manage-themes$$current-theme)
                         (THEME_DEFINITION-light manage-themes$$current-theme))))
    (manage-themes:load-theme current-theme (THEME_DEFINITION-package manage-themes$$current-theme)))
  (when (called-interactively-p 'interactive)
    (message (format "Theme set to %s" name))))

(defun manage-themes>reload-theme ()
  "Load configuration given the current values of jh/config."
  (interactive)
  (manage-themes>select-theme manage-themes/theme-name))

(defun manage-themes>save-theme ()
  "Save the theme custom variable"
  (interactive)
  (customize-save-variable 'manage-themes/theme-name manage-themes/theme-name))

(defun manage-themes>set-dark-mode ()
  "Set theme-mode to dark"
  (interactive)
  (customize-save-variable 'manage-themes/theme-mode "dark")
  (manage-themes>reload-theme)
  (normal-mode))

(defun manage-themes>set-light-mode ()
  "Set theme-mode to light"
  (interactive)
  (customize-save-variable 'manage-themes/theme-mode "light")
  (manage-themes>reload-theme)
  (normal-mode))

(defun manage-themes>toggle-high-contrast nil
  "Toggle high contrast 'modus' theme off and on."
  (interactive)
  (if manage-themes$$high-contrast-p
      (progn
        (setq manage-themes$$high-contrast-p nil
	      manage-themes$$high-contrast-old-theme manage-themes/theme-name)
        (manage-themes>select-theme "modus"))
    (progn
      (setq manage-themes$$high-contrast-p t)
      (manage-themes>select-theme manage-themes$$high-contrast-old-theme))))

(defun manage-themes>reset-theme ()
  (interactive)
  (reset-var 'manage-themes/theme-name)
  (reset-var 'manage-themes/theme-mode)
  (manage-themes>select-theme manage-themes/theme-name))

(add-hook 'emacsd-init@modules-loaded-hook
	      (lambda ()
            (message "SELECT THEME")
            (if (display-graphic-p)
                (manage-themes>select-theme manage-themes/theme-name)
              (manage-themes>select-theme "terminal"))
            
            ))


(provide 'manage-themes)
;;; manage-themes.el ends here
