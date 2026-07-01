;;; writing.el --- init writing-mode -*- lexical-binding: t; -*-

(straight-use-package 'mixed-pitch)
(straight-use-package 'olivetti)
(straight-use-package 'org-appear)
(straight-use-package 'org-modern)
(straight-use-package 'org-superstar)

(use-package wc-mode
  :straight t
  :custom
  (wc-modeline-format "[%tw words]")

  :config
  (unbind-key "C-c C-w" wc-mode-map)
)

(use-package contrib/hrs-writing-mode
  :straight nil
  :ensure nil
  :commands (hrs-writing-mode)
  :load-path "/home/chris/emacs/dev/2026-03-01--writing-mode/GOOD"
  :defer 1
;; TODO move  :hook (org-mode . writing-mode-repo)

  :config
  (require 'mixed-pitch)
  (require 'olivetti)
  ;; (require 'org-appear)
  ;; (require 'org-indent)
  ;; (require 'org-modern)
  ;; (require 'org-superstar)
  (require 'wc-mode)

;;; TODO move
  ;; when org, start hrs-writing-mode automatically if in directory
  ;; (defun writing-mode-repo ()
  ;;   (when (or (s-starts-with? (expand-file-name "~/documents/journal") buffer-file-name)
  ;;             (s-starts-with? (expand-file-name "~/documents/notes") buffer-file-name))
  ;;     (hrs-writing-mode 1)))

  (setq writing-enabled-modes
        '(
          ;; (org-mode . (org-appear-mode
          ;;              org-indent-mode
          ;;              org-modern-mode
          ;;              org-superstar-mode))
          (elfeed-show-mode . (mixed-pitch-mode
                               olivetti-mode))
          (special-mode . (mixed-pitch-mode
                           olivetti-mode))
          (text-mode . (flyspell-mode
                        mixed-pitch-mode
                        olivetti-mode
                        prettify-symbols-mode
                        visual-line-mode
                        wc-mode)))))

;;;###autoload
(defun my/toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
URL `http://xahlee.info/emacs/emacs/emacs_toggle_line_spacing.html'
Version 2017-06-02"
  (interactive)
  (if (or (null line-spacing) (< line-spacing 0.2))
      (setq line-spacing 0.5)
    (setq line-spacing 0.1))
  (redraw-frame (selected-frame)))

(provide 'mode/writing)
