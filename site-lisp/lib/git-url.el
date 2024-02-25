;;; -*- lexical-binding: t; -*-

(defun git-url-parse-to-author-project (git-url)
  "Parse a Git repository URL and return \=author~project\= format."
  (when (string-match "^\\(https?://\\|git@\\)?\\([^/]+\\)/\\(.+?\\)/\\(.+?\\)\\(/\\|$\\)" git-url)
    (let ((author (match-string 3 git-url))
          (project (match-string 4 git-url)))
      (format "%s~%s" author (string-remove-suffix ".git" project)))))

(provide 'lib/git-url)

