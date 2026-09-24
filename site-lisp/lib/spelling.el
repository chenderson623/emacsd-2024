;;; spelling.el -*- lexical-binding: t; -*-

(require 'transient)

;;;###autoload
(defun my-spelling>ispell-goto-next-error ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

;;;###autoload
(defun my-spelling>correct-previous-and-return ()
  "Correct the previous misspelled word and return cursor to typing position."
  (interactive)
  (let ((original-point (point))
        (found-error nil))
    ;; Search backward for a misspelled word with flyspell overlay
    (while (and (> (point) (point-min)) (not found-error))
      (backward-word 1)
      (let ((overlays (overlays-at (point))))
        (dolist (ov overlays)
          (when (overlay-get ov 'flyspell-overlay)
            (setq found-error t)))))
    ;; If found error, correct it interactively
    (if found-error
        (call-interactively #'flyspell-correct-at-point)
      (message "No misspelled word found before cursor"))
    (goto-char original-point)))

;;;###autoload
(defun my-spelling>toggle-flyspell-exclusive ()
  "Toggle `flyspell-mode', disabling `jinx-mode' if activating."
  (interactive)
  (if flyspell-mode
      (flyspell-mode -1) ; If on, turn it off.
    ;; If off, turn it on and disable the other.
    (when (and (fboundp 'jinx-mode) (bound-and-true-p jinx-mode))
      (jinx-mode -1))
    (flyspell-mode 1)))

;;;###autoload
(defun my-spelling>toggle-jinx-exclusive ()
  "Toggle `jinx-mode', disabling `flyspell-mode' if activating."
  (interactive)
  (if (not (fboundp 'jinx-mode))
      (message "Jinx is not available.")
    (if (bound-and-true-p jinx-mode)
        (jinx-mode -1) ; If on, turn it off.
      ;; If off, turn it on and disable the other.
      (when flyspell-mode
        (flyspell-mode -1))
      (jinx-mode 1))))

;;;###autoload
(defun my-spelling>avy-jinx-correct (pt &optional arg)
  "Correct word at point."
  (interactive "P")
  (let ((avy-all-windows)
        (current-prefix-arg (if arg 4)))
    (save-excursion (goto-char pt)
                    (call-interactively 'jinx-correct-word))))

;;;###autoload (autoload 'my-tmenu>spelling "lib/spelling" nil t)
(transient-define-prefix my-tmenu>spelling ()
  "Transient menu for switching spell checkers."
  ["Spelling"
   ["Spell Checkers"
    ("f" my-spelling>toggle-flyspell-exclusive :description (lambda () (my/transient-format-toggle "Flyspell" 'flyspell-mode)) :transient t)
    ("j" my-spelling>toggle-jinx-exclusive :description (lambda () (my/transient-format-toggle "Jinx" 'jinx-mode)) :transient t)]
   ["Flyspell"
    ("<" "flyspell-correct-previous" flyspell-correct-previous :transient t)
    (">" "flyspell-correct-next" flyspell-correct-next :transient t)
    ("p" "Correct previous & return" my-spelling>correct-previous-and-return)   ]
   ["Jinx"
    ("a" "avy-jinx-correct" my-spelling>avy-jinx-correct :transient t)]
   ])

(provide 'lib/spelling)
