;;; -*- lexical-binding: t; -*-

(require 'ts)

(defun this-week-range (&optional week-num)
  "Return timestamps \(BEG . END\) spanning the WEEK-NUM calendar work week.
If WEEK-NUM is not provided, use the current week."
  (let* ((now (ts-now))
         ;; Navigate to the date we need to
         (curr-week (string-to-number (ts-format "%W" now)))
         (days-to-adjust (if week-num (* 7 (- curr-week week-num)) 0))
         ;; We start by calculating the offsets for the beginning and
         ;; ending timestamps using the current day of the week.  Note
         ;; that the `ts-dow' slot uses the "%w" format specifier, which
         ;; counts from Sunday to Saturday as a number from 0 to 6.
         (adjust-beg-day (- (- (ts-dow now) 1)))
         (adjust-end-day (- 5 (ts-dow now)))
         ;; Make beginning/end timestamps based on `now', with adjusted
         ;; day and hour/minute/second values.  These functions return
         ;; new timestamps, so `now' is unchanged.
         (beg (thread-last now
                ;; `ts-adjust' makes relative adjustments to timestamps.
                (ts-adjust 'day (- adjust-beg-day days-to-adjust))
                ;; `ts-apply' applies absolute values to timestamps.
                (ts-apply :hour 0 :minute 0 :second 0)))
         (end (thread-last now
                (ts-adjust 'day (- adjust-end-day days-to-adjust))
                (ts-apply :hour 23 :minute 59 :second 59))))
    (cons beg end)))

(provide 'contrib/vedang-utility-functions)

