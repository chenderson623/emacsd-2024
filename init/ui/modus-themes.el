;;; -*- lexical-binding: t; -*-

(use-package
  modus-themes
  :straight (modus-themes :host gitlab :repo "protesilaos/modus-themes")
  :custom
  (modus-themes-slanted-constructs t)
  (modus-themes-diffs 'bg-only)
  (modus-themes-mode-line '(borderless accented))
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-headings '((t . (rainbow))))
  (modus-themes-markup '(bold italic intense))
  (modus-themes-bold-constructs t)
  (modus-themes-syntax '(alt-syntax))
  (modus-themes-prompts '(intense background))
  (modus-themes-completions
    '
    ((matches . (extrabold background intense))
      (selection . (semibold accented intense))
      (popup . (accented))))
  (modus-themes-paren-match '(bold underline intense))
  :init
  (if (display-graphic-p)
    (load-theme 'modus-operandi t)
    (load-theme 'modus-vivendi t)))

(provide 'ui/modus-themes)
