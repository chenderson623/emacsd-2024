;;; test-filepaths.el --- description -*- lexical-binding: t; -*-
;;
;; Author: chris <chris@penguin>
;; Created: 17 Dec 2023
;;
;;
;;; Commentary:
;;
;;; Code:

(load-file (expand-file-name "elisp/filepaths.el" emacs-d))

(expand-file-name "~/.config")
(expand-file-name "emacs")
(expand-file-name "emacs" "~/.config")
(expand-file-name "emacs" "~/.configX")
(expand-file-name "emacs" "~/.config/")

(filepath:assert-dir-exists "~/.config")
(filepath:assert-dir-exists "~/.configX")

(filepath:assert-dir-exists "~/.config" "emacs")
(filepath:assert-dir-exists "~/.config/" "emacs")
(filepath:assert-dir-exists "~/.config" "emacsX")
(filepath:assert-dir-exists "~/.configX" "emacs")

(filepath:join-paths "~/.configs" "emacs" "somefile")

(filepath:ensure-dir-exists "~/temp" "emacs")
(filepath:assert-dir-exists "~/tempx" "emacsX")
(filepath:ensure-dir-exists "~/temp" "emacs/some/sub/dir")

(provide 'test-filepaths)
;;; test-filepaths.el ends here
