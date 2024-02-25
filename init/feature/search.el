;;; search.el --- setup search -*- lexical-binding: t; -*-

(use-package isearch
  :straight (:type built-in)
  :bind (:map isearch-mode-map
              ;; consistent with ivy-occur
              ("C-c C-o"                   . isearch-occur)
              ([escape]                    . isearch-cancel)
              ;; Edit the search string instead of jumping back
              ([remap isearch-delete-char] . isearch-del-char))
  :config
  (define-advice isearch-occur (:after (_regexp &optional _nlines))
    (isearch-exit))
  :custom
  ;; Record isearch in minibuffer history, so C-x ESC ESC can repeat it.
  (isearch-resume-in-command-history t)
  ;; One space can represent a sequence of whitespaces
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace t)
  (isearch-repeat-on-direction-change t)
  ;; M-< and M-> move to the first/last occurrence of the current search string.
  (isearch-allow-motion t)
  (isearch-motion-changes-direction t)
  ;; lazy isearch
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format " [%s/%s]")
  (lazy-highlight-buffer t)
  ;; Mimic Vim
  (lazy-highlight-cleanup nil))

(use-package rg
  :if (executable-find "rg") 
  :straight t
  :defer t
  :commands (rg rg>vc-or-dir rg>ref-in-dir emacsd-dir>rg)
  ;; :bind (("M-s M-g" . #'rg>vc-or-dir)
  ;;        ("M-s M-." . #'rg>ref-in-dir)
  ;;        :map rg-mode-map
  ;;        ("s" . #'rg>save-search-as-name)
  ;;        ("C-n" . next-line)
  ;;        ("C-p" . previous-line)
  ;;        ("M-n" . rg-next-file)
  ;;        ("M-p" . rg-prev-file))
  :config
  (setq rg-custom-type-aliases nil)
  (setq rg-group-result t)
  (setq rg-show-columns t)
  (setq rg-command-line-flags '("--hidden"))
  (rg-define-toggle "--context 3" (kbd "C"))
  (rg-define-toggle "-A 5" (kbd "A"))

  (rg-define-search rg>vc-or-dir
    "RipGrep in project root or present directory."
    :query ask
    :format regexp
    :files "everything"
    :dir (or (project-root (project-current))
	     (vc-root-dir)              ; search root project dir
	     default-directory)         ; or from the current dir
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (rg-define-search rg>ref-in-dir
    "RipGrep for thing at point in present directory."
    :query point
    :format regexp
    :files "everything"
    :dir default-directory
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (rg-define-search emacsd-dir>rg
    "RipGrep in .emacs.d directory."
    :format regexp
    :files "everything"
    :dir "~/.emacs.d"
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (defun rg>save-search-as-name ()
    "Save `rg' buffer, naming it after the current search query.
         This function is meant to be mapped to a key in `rg-mode-map'."
    (interactive)
    (let ((pattern (car rg-pattern-history)))
      (rg-save-search-as-name (concat "«" pattern "»"))))
  )

(provide 'feature/search)
