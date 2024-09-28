;;; -*- lexical-binding: t; -*-

(use-package persist
  :straight t
  :custom
  (persist--directory-location (emacs-state*filepath "persist")))

;; https://github.com/bbatsov/crux
(use-package crux
  :straight t
  :bind (
         ("C-a" . crux-move-beginning-of-line)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-x K" . crux-kill-other-buffers)
         ("C-k" . crux-smart-kill-line))
	 ;;("C-c o" . crux-open-with)
         ;;("M-o" . crux-smart-open-line)
         ;;("C-c n" . crux-cleanup-buffer-or-region)
         ;;("C-c f" . crux-recentf-find-file)
         ;;("C-M-z" . crux-indent-defun)
         ;;("C-c u" . crux-view-url)
         ;;("C-c e" . crux-eval-and-replace)
         ;;("C-c w" . crux-swap-windows)
         ;;("C-c D" . crux-delete-file-and-buffer)
         ;;("C-c r" . crux-rename-buffer-and-file)
         ;;("C-c t" . crux-visit-term-buffer)
         ;;("C-c k" . crux-kill-other-buffers)
         ;;("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ;;("C-c I" . crux-find-user-init-file)
         ;;("C-c S" . crux-find-shell-init-file)
         ;;("s-r" . crux-recentf-find-file)
         ;;("s-j" . crux-top-join-line)
         ;;("C-^" . crux-top-join-line)
         ;;("s-k" . crux-kill-whole-line)
         ;;("C-<backspace>" . crux-kill-line-backwards)
         ;;("s-o" . crux-smart-open-line-above)
         ;;([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ;;("C-c s" . crux-ispell-word-then-abbrev)
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(provide 'emacs/extensions)

