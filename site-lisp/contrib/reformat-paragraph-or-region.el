;;; reformat-paragraph-or-region.el --- reformat paragraph -*- lexical-binding: t; -*-

;; adopted from: https://sachachua.com/blog/2025/09/emacs-cycle-through-different-paragraph-formats-all-on-one-line-wrapped-max-one-sentence-per-line-one-sentence-per-line/
(defvar sc-repeat-counter '()
  "How often `sc-repeat-next' was called in a row using the same command.
This is an alist of (cat count list) so we can use it for different functions.")

;;;###autoload
(defun sc-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

;;;###autoload
(defun sc-fill-paragraph-semlf-long ()
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph-semlf)))

(defun sc-repeat-next (category &optional element-list reset)
  "Return the next element for CATEGORY.
Initialize with ELEMENT-LIST if this is the first time."
  (let* ((counter
          (or (assoc category sc-repeat-counter)
              (progn
                (push (list category -1 element-list)
                      sc-repeat-counter)
                (assoc category sc-repeat-counter)))))
    (setf (elt (cdr counter) 0)
          (mod
           (if reset 0 (1+ (elt (cdr counter) 0)))
           (length (elt (cdr counter) 1))))
    (elt (elt (cdr counter) 1) (elt (cdr counter) 0))))

(defun sc-in-prefixed-comment-p ()
  (or (member 'font-lock-comment-delimiter-face (face-at-point nil t))
      (member 'font-lock-comment-face (face-at-point nil t))
      (save-excursion
        (beginning-of-line)
        (comment-search-forward (line-end-position) t))))

;; It might be nice to figure out what state we're
;; in and then cycle to the next one if we're just
;; working with a single paragraph. In the
;; meantime, just going by repeats is fine.
;;;###autoload
(defun sc-reformat-paragraph-or-region ()
  "Cycles the paragraph between three states: filled/unfilled/fill-sentences.
If a region is selected, handle all paragraphs within that region."
  (interactive)
  (let ((func (sc-repeat-next 'sc-reformat-paragraph
                              '(fill-paragraph sc-unfill-paragraph fill-paragraph-semlf
                                               sc-fill-paragraph-semlf-long)
                              (not (eq this-command last-command))))
        (deactivate-mark nil))
    (if (region-active-p)
        (save-restriction
          (save-excursion
            (narrow-to-region (region-beginning) (region-end))
            (goto-char (point-min))
            (while (not (eobp))
              (skip-syntax-forward " ")
              (let ((elem (and (derived-mode-p 'org-mode)
                               (org-element-context))))
                (cond
                 ((eq (org-element-type elem) 'headline)
                  (org-forward-paragraph))
                 ((member (org-element-type elem)
                          '(src-block export-block headline property-drawer))
                  (goto-char
                   (org-element-end (org-element-context))))
                 (t
                  (funcall func)
                  (if fill-forward-paragraph-function
                      (funcall fill-forward-paragraph-function)
                    (forward-paragraph))))))))
      (save-excursion
        (move-to-left-margin)
        (funcall func)))))

(provide 'contrib/reformat-paragraph-or-region)
;;; reformat-paragraph-or-region.el ends here
