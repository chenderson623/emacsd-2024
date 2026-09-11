;;; -*- lexical-binding: t; -*-

;; https://github.com/nullman/emacs-org-visibility
(use-package org-visibility
  :straight t
  :after (org)
  ;;:bind (:map org-visibility-mode-map
  ;;             ("C-x C-v" . org-visibility-force-save) ; defaults to `find-alternative-file'
  ;;             ("C-x M-v" . org-visibility-remove))    ; defaults to undefined
  :hook (org-mode . org-visibility-mode)
  :custom
  ;; optionally change the location of the state file
  (org-visibility-state-file `,(emacs-state*filepath "org-visibility-state"))
  ;; list of directories and files to persist and restore visibility state of
  ;; (org-visibility-include-paths `(,(file-truename "~/.emacs.d/init-emacs.org")
  ;;                                 ,(file-truename "~/org")))
  ;; persist all org files regardless of location
  ;; (org-visibility-include-regexps '("\\.org\\'"))
  ;; list of directories and files to not persist and restore visibility state of
  ;;(org-visibility-exclude-paths `(,(file-truename "~/org/old")))
  ;; optionally set maximum number of files to keep track of
  ;; oldest files will be removed from the state file first
  (org-visibility-maximum-tracked-files 100)
  ;; optionally set maximum number of days (since saved) to keep track of
  ;; files older than this number of days will be removed from the state file
  (org-visibility-maximum-tracked-days 180)
  ;; optionally turn off visibility state change messages
  ;;(org-visibility-display-messages nil)
  :config
  ;; Emacs 31 removed date-to-time's timezone-make-date-arpa-standard fallback.
  ;; Upstream org-visibility used "%FT%T%Z", which on Windows becomes
  ;; "2026-09-10T13:14:50US Mountain Standard Time" -- invalid ISO 8601 and
  ;; no longer parseable. Use a numeric offset, and tolerate old state entries.
  (defun org-visibility--timestamp ()
    "Return timestamp in ISO 8601 format (YYYY-mm-ddTHH:MM:SS+/-hhmm)."
    (format-time-string "%FT%T%z"))

  (defun org-visibility--timestamp-to-time (timestamp)
    "Parse TIMESTAMP from org-visibility state into a time value."
    (condition-case nil
        (date-to-time timestamp)
      (error
       ;; Old Windows %Z form: strip the named zone and treat as local time.
       (if (string-match
            "\\`\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)"
            timestamp)
           (date-to-time (match-string 1 timestamp))
         (error "Invalid date: %s" timestamp)))))

  (defun org-visibility--timestamp-to-epoch (timestamp)
    "Return epoch (seconds since 1970-01-01) from TIMESTAMP."
    (truncate (float-time (org-visibility--timestamp-to-time timestamp))))

  (defun org-visibility--remove-over-maximum-tracked-days (data)
    "Remove all files over maximum day count from DATA.

Does nothing unless `org-visibility-maximum-tracked-days' is
non-nil and exceeded."
    (if (and org-visibility-maximum-tracked-days
             (cl-plusp org-visibility-maximum-tracked-days))
        (cl-do ((day (- (time-to-days (current-time))
                        org-visibility-maximum-tracked-days))
                (d data (cdr d))
                (n 0 (1+ n)))
            ((or (null d)
                 (< (time-to-days
                     (org-visibility--timestamp-to-time (cadar d)))
                    day))
             (cl-subseq data 0 n)))
      data)))

(provide 'mode/extended/org-folding)
