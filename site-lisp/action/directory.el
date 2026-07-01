;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun directory-action/create-timestamped-directory (directory-name)
  "Create a new directory with a timestamp prefix and sanitized name."
  (interactive "sEnter directory name: ")
  (let* ((sanitized-name (replace-regexp-in-string "[^A-Za-z0-9_-]" "-" (replace-regexp-in-string " " "_" directory-name)))
         (timestamp (format-time-string "%Y-%m-%d-%H%M%S"))
         (new-directory (concat timestamp "-" sanitized-name)))
    (make-directory new-directory)
    (message "Created directory: %s" new-directory)))

;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))

;;;###autoload
(defun my/fd-dired-recent-files (dir)
  "Recursively list the 20 most recently modified files in DIR using fd."
  (interactive "DDirectory: ")
  (let* ((fd-executable (or (executable-find "fd")
                            (executable-find "fdfind")))
         (fd-cmd (format "%s . %s --type f --changed-within 10y --exec stat --format '%%Y %%n' | sort -rn | head -n 20 | cut -d' ' -f2-" fd-executable (shell-quote-argument (expand-file-name dir))))
         (process-buffer (generate-new-buffer " *fd-output*"))
         (exit-code (call-process-shell-command fd-cmd nil process-buffer)))
    (unless fd-executable
      (user-error "Neither 'fd' nor 'fdfind' command found. Please install fd or check your exec-path."))
    (unwind-protect
        (cond
         ((= exit-code 0)
          (with-current-buffer process-buffer
            (let ((files (split-string (buffer-string) "\n" 'omit-empty)))
              (if files
                  (dired (cons "Recent Files (fd)" files))
                (message "No files found.")))))
         ((= exit-code 127)
          (user-error "fd command not found during execution. Check your shell's PATH from Emacs."))
         (t
          (display-buffer process-buffer)
          (user-error "fd command failed with exit code %d. See %s for details."
                      exit-code (buffer-name process-buffer))))
      (kill-buffer process-buffer))))

(provide 'action/directory)
;;; directory-functions.el ends here
