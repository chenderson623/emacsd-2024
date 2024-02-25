;;; -*- lexical-binding: t; -*-

;;;; Org Functions
;;;;; Org Emphasis Functions
;; Adapted from https://emacs.stackexchange.com/a/14586
;; See https://emacstil.com/til/2021/11/29/org-emphasize-dwim/

;;;###autoload
(defun org-emphasize-dwim (&optional char)
  (interactive)
  (unless (region-active-p)
    (lem-maybe-mark-word))
  (org-emphasize char))

;;;###autoload
(defun org-emphasize-with-verbatim-dwim ()
  (interactive)
  (org-emphasize-dwim ?=))

;;;###autoload
(defun org-emphasize-with-code-dwim ()
  (interactive)
  (org-emphasize-dwim ?~))

(defun lem--cursor-outside-of-any-word ()
  (not (bounds-of-thing-at-point 'word)))

(defun lem--cursor-at-beginning-of-a-word ()
  (eq (point) (car (bounds-of-thing-at-point 'word))))

;;;###autoload
(defun lem-maybe-mark-word ()
  "Mark the current word. If cursor is outside of a word bounds, mark the empty position."
  (interactive)
  (unless (or (lem--cursor-outside-of-any-word) (lem--cursor-at-beginning-of-a-word))
    (backward-word))
  (unless (lem--cursor-outside-of-any-word)
    (mark-word)))

;;;;; Narrow & Advance/Retreat
;; Functions to advance forwards or backwards through narrowed tree
;;;###autoload
(defun lem-org-advance ()
  (interactive)
  (when (buffer-narrowed-p)
    (goto-char (point-min))
    (widen)
    (org-forward-heading-same-level 1))
  (org-narrow-to-subtree))

;;;###autoload
(defun lem-org-retreat ()
  (interactive)
  (when (buffer-narrowed-p)
    (goto-char (point-min))
    (widen)
    (org-backward-heading-same-level 1))
  (org-narrow-to-subtree))

;;;;; Clone and Narrow
;;;###autoload
(defun lem-clone-buffer-and-narrow ()
  "Clone buffer and narrow outline tree"
  (interactive)
  (let ((buf (clone-indirect-buffer-other-window nil nil)))
    (with-current-buffer buf
      (cond ((derived-mode-p 'org-mode)
             (org-narrow-to-element))
            ((derived-mode-p 'markdown-mode)
             (markdown-narrow-to-subtree))))
    (switch-to-buffer-other-window buf)))

(provide 'contrib/lem-org-functions)
