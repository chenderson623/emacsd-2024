;; -*- lexical-binding: t -*-

;;;###autoload
(defun og-action>replace-quote-block-with-elisp-block ()
  (interactive)
  (er/mark-org-code-block)
  (replace-string-in-region "#+BEGIN_QUOTE" "#+BEGIN_SRC emacs-lisp")
  (replace-string-in-region "#+END_QUOTE" "#+END_SRC")
)

;;;###autoload
(defun org-action>move-region-or-subtree-to-other-window ()
  (interactive)
  (when (and
         (eq 'org-mode major-mode)
         (not (region-active-p)))
    (org-mark-subtree))
  (call-interactively 'organizing:move-region-to-other-window))

;; adopted from https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
;;;###autoload
(defun org-action>delete-url-from-link ()
  "Remove the link part of an org-mode link at point and keep
only the description"
  (interactive)
  (let ((elem (org-element-context)))
    (if (eq (car elem) 'link)
        (let* ((content-begin (org-element-property :contents-begin elem))
               (content-end  (org-element-property :contents-end elem))
               (link-begin (org-element-property :begin elem))
               (link-end (org-element-property :end elem)))
          (if (and content-begin content-end)
              (let ((content (buffer-substring-no-properties content-begin content-end)))
                (delete-region link-begin link-end)
                (insert content)))))))

;; copy property
;;;###autoload
(defun org-action>copy-id-property ()
  "copy the id property of current heading"
  (interactive)
  (kill-new (org-entry-get (point) "ID")))

;;;###autoload
(defun +org>rename-header (label)
  "Rename the current section's header to LABEL, and moves the
point to the end of the line."
  (interactive (list
                (read-string "Header: "
                             (substring-no-properties (org-get-heading t t t t)))))
  (org-back-to-heading)
  (replace-string (org-get-heading t t t t) label))

;;;###autoload
(defun +navigate>regex-on-line (regexp)
  "Move point to the end of the first match of REGEXP on the current line.
If no match is found, point remains unchanged."
  (interactive "sRegexp: ")
  (let ((line-end (line-end-position)))
    (re-search-forward regexp line-end t)))

;;;###autoload
(defun +navigate>org-link-on-line ()
  (interactvive)
  (+navigate>regex-on-line "\\[\\[")
  )

;;;###autoload
(defun +org>back-to-heading ()
  (interactive)
  (org-back-to-heading t)
  )

;;;###autoload
(defun +org>goto-headline-text ()
  (interactive)
  (let ((text (nth 4 (org-heading-components))))
    (end-of-line)
    (when text
     (search-backward text))))

;;;###autoload
(defun +org>prepend-headline-with-date ()
  "Prepend an inactive timestamp from the :CREATED: property (or today if missing) to the headline, formatted as [YYYY-MM-DD Day]."
  (interactive)
  (let* ((created-prop (org-entry-get (point) "CREATED"))
         (time (if created-prop
                   (apply #'encode-time (org-parse-time-string created-prop))
                 (current-time)))
         (formatted-date (format-time-string "[%Y-%m-%d %a]" time))
         (new-headline (concat formatted-date " " (org-get-heading))))
    (org-edit-headline new-headline)))

;;;###autoload
(defun +org>convert-quote-to-src-block (lang)
  "Convert the QUOTE block at point to a SRC block of language LANG."
  (interactive "sLanguage for src block: ")
  (save-excursion
    (let ((case-fold-search nil)
          beg end)
      ;; Find the beginning of the QUOTE block
      (when (org-in-block-p '("QUOTE"))
        (search-backward-regexp "^#\\+BEGIN_QUOTE" nil t))
      (when (looking-at "^#\\+BEGIN_QUOTE")
        (setq beg (point))
        ;; Find the end of the QUOTE block
        (if (search-forward-regexp "^#\\+END_QUOTE" nil t)
            (setq end (point))
          (error "No matching #+END_QUOTE found"))
        ;; Replace block markers
        (goto-char beg)
        (when (looking-at "^#\\+BEGIN_QUOTE")
          (replace-match (concat "#+BEGIN_SRC " lang) t))
        (goto-char end)
        (forward-line 0)
        (when (looking-at "^#\\+END_QUOTE")
          (replace-match "#+END_SRC"))))))

;;;###autoload
(defun +org>convert-quote-to-emacs-lisp-block ()
  (interactive)
  (+org>convert-quote-to-src-block "emacs-lisp")
  )

;;;###autoload
(defun my-refile-refactor>org-edit-headline ()
    "Edit the current Org headline:
- Remove any tags.
- Remove 'at [hash]' if present.
- If headline contains '·', transpose text before and after '·'."
    (interactive)
    (org-back-to-heading t)
            (let* ((element (org-element-at-point))
                         (title (org-element-property :raw-value element))
                         (tags (org-element-property :tags element)))
                ;; Remove tags
                (when tags
                    (org-set-tags nil))
                ;; Remove 'at [hash]' if present
                (setq title (replace-regexp-in-string " at [a-f0-9]+" "" title))

                (setq title (replace-regexp-in-string " · GitHub" "" title))

                ;; Transpose around '·' if present
                (when (string-match "\\(.*\\) · \\(.*\\)" title)
                    (let ((before (match-string 1 title))
                                (after (match-string 2 title)))
                        (setq title (format "%s · %s" after before))))
                (org-edit-headline title)))


(provide 'action/org)
;;; org.el ends here
