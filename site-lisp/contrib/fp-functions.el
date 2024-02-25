;;; -*- lexical-binding: t; -*-

;; adopted from https://github.com/dominiksta/dotfiles2
(defun fp/rg-search-full-command (default-directory command)
  (let ((default-directory default-directory))
    (compilation-start command 'rg-mode #'rg-buffer-name)))

(defun fp/rg-search-multi-directory (base-dir dirs search)
  (fp/rg-search-full-command
    base-dir
    (format
      "rg -S --color=always --colors=match:fg:red \
      --colors=path:fg:magenta --colors=line:fg:green --colors=column:none -n \
      --column --heading --no-config -e \"%s\" %s"
      search
      (mapconcat 'shell-quote-argument dirs " "))))

(defun fp/rg-search-multi-directory-thing-at-point (base-dir dirs)
  (let*
    (
      (default (word-at-point t))
      (in (read-string (format "Search (default %s): " default)))
      (search
        (if (eq (length in) 0)
          default
          in)))
    (fp/rg-search-multi-directory base-dir dirs search)))

(provide 'contrib/fp-functions)
