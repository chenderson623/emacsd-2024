;;; org.el --- custom org functions -*- lexical-binding: t; -*-

(require 'dash)
(require 'cl-lib)

;;;###autoload
(defun org-lib/org-filename-from-title
    (title)
  "Creates a useful filename based on a header string, TITLE.
For instance, given the string:    What's all this then?
     This function will return:    whats-all-this-then"
  (interactive "s")
  (let* ((no-letters (rx (one-or-more (not alphanumeric))))
         (init-try (->> title
                        capitalize
                        (replace-regexp-in-string "'" "")
                        (replace-regexp-in-string no-letters "-"))))
    (string-trim init-try "-+" "-+")))

;;;###autoload
(defun org-lib/org-tagsafe (tag-text)
  (upcase (replace-regexp-in-string "-" "" tag-text)))

;; This is modified from ha-org-create-org-file
;;;###autoload
(defun org-lib/create-org-file-from-props (filepath header body tags properties)
    "Create a new Org file by FILEPATH. The contents of the file is
    pre-populated with the HEADER, BODY and any associated TAGS."
    (find-file-other-window filepath)
    (org-set-file-property "TITLE" (read-string "TITLE: " header) t)
    (when tags
      (org-set-file-property "FILETAGS" (s-join " " tags)))

    ;; Insert any drawer properties as #+PROPERTY entries:
    (when properties
      (goto-char (point-min))
      (or (re-search-forward "^\s*$" nil t) (point-max))
      (--map (insert (format "#+PROPERTY: %s %s \n" (cl-first it) (cl-second it))) properties))

    ;; My auto-insert often adds an initial headline for a subtree, and in this
    ;; case, I don't want that... Yeah, this isn't really globally applicable,
    ;; but it shouldn't cause a problem for others.
    (when (re-search-forward "^\\* [0-9]$" nil t)
      (replace-match ""))

    (delete-blank-lines)
    (goto-char (point-max))
    (insert "\n")
    (insert body))

;;;###autoload
(defun my-org>org-unwrap-block ()
  "Remove the #+BEGIN and #+END delimiters from the current block, leaving text."
  (interactive)
  ;; org-element-lineage climbs up the AST tree to find a matching block type
  (let* ((current-element (org-element-at-point))
         (element (org-element-lineage
                   current-element
                   '(quote-block special-block src-block example-block verse-block export-block)
                   t)))
    (if element
        (let* ((post-blank (org-element-property :post-blank element))
               (block-begin (org-element-property :begin element))
               (contents-begin (org-element-property :contents-begin element))
               (contents-end (org-element-property :contents-end element)))
          (if (and contents-begin contents-end)
              (save-excursion
                (let ((inner-text (buffer-substring-no-properties contents-begin contents-end)))
                  ;; Delete the entire block structural region
                  (delete-region block-begin (org-element-property :end element))
                  ;; Insert the inner text right back
                  (goto-char block-begin)
                  (insert inner-text)
                  ;; Retain exact blank space padding if any existed after the block
                  (when (> post-blank 0)
                    (save-excursion (insert (make-string post-blank ?\n))))
                  (message "Block safely unwrapped into regular text.")))
            (message "Block has no inner content to unwrap.")))
      (message "Not inside a supported Org block."))))

(provide 'lib/org)
