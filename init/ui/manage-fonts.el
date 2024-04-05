
;; These can be pre-set in local init
  ;; (defvar my$fontaine-default-font-height
  ;;   (if (> (x-display-pixel-width) 2500)
  ;;       160 140)

(defvar my$fontaine-default-preset 'laptop)

;; TODO create a transient menu for fontaine
;; C-c u f ? C-C u for "ui"
;; (use-package helpful
;;   :bind
;;   ("M-." . helpful-at-point)
;;   ("s-h" . my/helpful-menu)
;;   :straight t
;;   :config
;;   (transient-define-prefix my/helpful-menu ()
;;     "Return a `transient' compliant list to apply to different transients."
;;     ["Help"
;;      ""
;;      ("Q" "Kill Helpful Buffers" helpful-kill-buffers)
;;      ""
;;      ("b" "Bindings" embark-bindings)
;;      ("c" "Command" helpful-command)
;;      ("f" "Function (interactive)" helpful-callable)
;;      ("F" "Function (all)" helpful-function)
;;      ("k" "Key" helpful-key)
;;      ("l" "Library" find-library)
;;      ("m" "Macro" helpful-macro)
;;      ("p" "Thing at point" helpful-at-point)
;;      ("." "Thing at point" helpful-at-point)
;;      ("t" "Text properties" describe-text-properties)
;;      ("v" "Variable" helpful-variable)]))


(use-package fontaine
  :straight t
  :commands (fontaine-set-preset) ;; this is called below and will load fontaine
  :custom 
  (fontaine-latest-state-file
   (emacs-state*filepath "fontaine-latest-state.eld"))

  ;; The `:inherit' contains the name of another named preset.
  (fontaine-presets
   '((tiny
      :default-family "Iosevka Comfy Wide Fixed"
      :default-height 70)
     (small
      :default-family "Iosevka Comfy Fixed"
      :default-height 90)
     (regular
      :default-height 100)
     (medium
      :default-height 110)
     (laptop
      :default-family "Source Code Pro"
      :default-weight semilight
      :default-height 100
      :bold-weight extrabold)
     (large
      :default-weight semilight
      :default-height 140
      :bold-weight extrabold)
     (presentation
      :default-weight semilight
      :default-height 170
      :bold-weight extrabold)
     (jumbo
      :default-weight semilight
      :default-height 220
      :bold-weight extrabold)
     (t
      ;; I keep all properties for didactic purposes, but most can be
      ;; omitted.  See the fontaine manual for the technicalities:
      ;; <https://protesilaos.com/emacs/fontaine>.
      :default-family "Iosevka Comfy"          ;; TODO set this
      :default-weight regular
      :default-height 100                      ;; TODO set this
      :fixed-pitch-family nil ; falls back to :default-family
      :fixed-pitch-weight nil ; falls back to :default-weight
      :fixed-pitch-height 1.0
      :fixed-pitch-serif-family nil ; falls back to :default-family
      :fixed-pitch-serif-weight nil ; falls back to :default-weight
      :fixed-pitch-serif-height 1.0
      :variable-pitch-family "Iosevka Comfy Duo"  ;; TODO set this
      :variable-pitch-weight nil
      :variable-pitch-height 1.0
      :bold-family nil ; use whatever the underlying face has
      :bold-weight bold
      :italic-family nil
      :italic-slant italic
      :line-spacing nil))))

(defun init-fontaine()
  ;; Recover last preset or fall back to desired style from `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) my$fontaine-default-preset))

  ;; Save fontaine preset on shutdown
  ;; TODO add keybinding for this, don't do it automatically
  ;; TODO create function to delete the preset state file
  ;;(add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  (define-key global-map (kbd "C-c f") #'fontaine-set-preset)
  (define-key global-map (kbd "C-c F") #'fontaine-set-face-font)

  ;; Set up the `after-enable-theme-hook'
  (defvar after-enable-theme-hook nil
    "Normal hook run after enabling a theme.")

  (defun run-after-enable-theme-hook (&rest _args)
    "Run `after-enable-theme-hook'."
    (run-hooks 'after-enable-theme-hook))

  (advice-add 'enable-theme :after #'run-after-enable-theme-hook)

  (add-hook 'after-enable-theme-hook
            (lambda ()
              (fontaine-set-preset fontaine-current-preset)
              ;;(my-setup-org-fonts)
              )))

;; TODO this belongs in the org use-package definition
(defun my-setup-org-fonts()
  (let* ((variable-tuple

          (cond ((x-list-fonts "Iosevka Comfy")   '(:font "Iosevka Comfy"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (headline `( :weight bold)))


    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.4))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))
        
     )))

(if (display-graphic-p)
    (init-fontaine))

(provide 'ui/manage-fonts)

;; (custom-theme-set-faces
;;      'user
;;      `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;      `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
;;      `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
;;      `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.4))))
;;      `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))

;;      ;; `(org-level-1 ((t (:foreground "yellow"))))
;;      ;; `(org-level-2 ((t (:foreground "blue"))))
;;      ;; `(org-level-3 ((t (:foreground "magenta"))))
;;      ;; `(org-level-4 ((t (:foreground ,green))))
;;      ;; `(org-level-5 ((t (:foreground ,purple+1))))
;;      ;; `(org-level-6 ((t (:foreground ,orange+1))))
;;      ;; `(org-level-7 ((t (:foreground ,red+1))))
;;      ;; `(org-level-8 ((t (:foreground ,blue+1))))
     
;;      )
