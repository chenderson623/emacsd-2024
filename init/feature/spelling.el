;;; -*- lexical-binding: t; -*-

;;(defvar ispell-program-name (or (executable-find "ispell")
;;                               (executable-find "aspell")
;;                               (executable-find "hunspell")))

;;(when ispell-program-name
;;  (add-hook 'text-mode-hook #'flyspell-mode)
;;  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

;;(setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))

;;(setq flyspell-use-meta-tab nil)

(use-package ispell
  :commands (ispell-word ispell-region ispell-buffer)
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")
    ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

(use-package flyspell
  :config
  (setq flyspell-abbrev-p t
        flyspell-use-global-abbrev-table-p t
        flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  :hook ((markdown-mode . flyspell-mode)
         (org-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;; completion of spellings
;; https://github.com/d12frosted/flyspell-correct
(use-package flyspell-correct
  :straight t
  :after flyspell
  :bind (:map flyspell-mode-map
         ("C-;" . flyspell-correct-previous)
         ("C-:" . flyspell-correct-at-point))
  :custom
  (flyspell-correct-interface #'flyspell-correct-completing-read))

;; use avy interface for flyspell
;; https://github.com/d12frosted/flyspell-correct
(use-package flyspell-correct-avy-menu
  :after flyspell-correct)



(use-package jinx
  :straight t
  :demand t
  :commands (jinx-mode jinx-correct))

;;;;; Spelling Goto Next Error
(defun my-spelling>ispell-goto-next-error ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

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

(defun my/toggle-flyspell-exclusive ()
  "Toggle `flyspell-mode', disabling `jinx-mode' if activating."
  (interactive)
  (if flyspell-mode
      (flyspell-mode -1) ; If on, turn it off.
    ;; If off, turn it on and disable the other.
    (when (and (fboundp 'jinx-mode) (bound-and-true-p jinx-mode))
      (jinx-mode -1))
    (flyspell-mode 1)))

(defun my/toggle-jinx-exclusive ()
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

(transient-define-prefix my-transient>spellcheck-menu ()
  "Transient menu for switching spell checkers."
  ["Spelling"
  ["Spell Checkers"
   ("f" my/toggle-flyspell-exclusive :description (lambda () (my/transient-format-toggle "Flyspell" 'flyspell-mode)) :transient t)
   ("j" my/toggle-jinx-exclusive :description (lambda () (my/transient-format-toggle "Jinx" 'jinx-mode)) :transient t)]
  ["Flyspell"
   ("<" "flyspell-correct-previous" flyspell-correct-previous :transient t)
   (">" "flyspell-correct-next" flyspell-correct-next :transient t)   
   ("p" "Correct previous & return" my-spelling>correct-previous-and-return)   ]
]

  )

    ;; ("q" nil)
    ;; ("<" flyspell-correct-previous :color pink)
    ;; (">" flyspell-correct-next :color pink)
    ;; ("c" ispell)
    ;; ("d" ispell-change-dictionary)
    ;; ("f" flyspell-buffer :color pink)
    ;; ("m" flyspell-mode)))

     ;; ("c" "Jinx correct" jinx-correct)]


(provide 'feature/spellcheck)

