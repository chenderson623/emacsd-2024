
(defvar my$fontaine-default-font-height
   (if (> (x-display-pixel-width) 2500)
       160 140))

(defvar my$fontaine-default-preset 'laptop)

(use-package fontaine
  :straight t
  :commands (fontaine-set-preset) ;; this is called below and will load fontaine
  :init 
  (setq fontaine-latest-state-file
   (emacs-state*filepath "fontaine-latest-state.eld"))

  ;; The `:inherit' contains the name of another named preset.
  (setq fontaine-presets
   '((tiny
      :default-family "Aporetic Sans Mono"
      :default-height 70)
     (small
      :default-family "Aporetic Sans Mono"
      :default-height 90)
     (regular
      :default-height 100)
     (medium
      :default-height 110)
     (laptop
      :default-family "Cascadia Code NF"
      :variable-pitch-family "Arial"
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
      :default-family "Aporetic Sans Mono"          ;; TODO set this
      :default-weight regular
      :default-height 100                      ;; TODO set this
      :fixed-pitch-family nil ; falls back to :default-family
      :fixed-pitch-weight nil ; falls back to :default-weight
      :fixed-pitch-height 1.0
      :fixed-pitch-serif-family nil ; falls back to :default-family
      :fixed-pitch-serif-weight nil ; falls back to :default-weight
      :fixed-pitch-serif-height 1.0
      :variable-pitch-family "Aporetic Sans"  ;; TODO set this
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

  ;; Persist the latest font preset when closing/starting Emacs and
  ;; while switching between themes.
  (fontaine-mode 1)
)

(if (display-graphic-p)
    (init-fontaine))

(provide 'ui/manage-fonts)
