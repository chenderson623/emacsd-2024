;;; -*- lexical-binding: t; -*-

(require 'lib/git-url)

;; adopted from: https://xenodium.com/emacs-clone-git-repo-from-clipboard/
;; change to prompt for a target directory and to name the target directory author~project
;;;###autoload
(defun git-action>clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (clone-parent-dir (read-directory-name "Target parent directory: "))
	 (clone-dir-name (git-url-parse-to-author-project url))		   
         (clone-path (concat (file-name-as-directory clone-parent-dir)
                              clone-dir-name))
         (default-directory clone-parent-dir)
         (command (format "git clone %s %s --depth=1" url clone-dir-name))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (when (file-exists-p clone-path)
      (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
          (delete-directory clone-path t)
        (user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory clone-parent-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process state)
                                 (let ((output (with-current-buffer (process-buffer process)
                                                 (buffer-string))))
                                   (kill-buffer (process-buffer process))
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "finished: %s" command)
                                         (dired clone-path))
                                     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))

(provide 'action/git-clone)

