;;; variable-pitch-buffer-face-mode.el --- description -*- lexical-binding: t; -*-

;;; Commentary:
;; Design Concepts:
;; 1. Defaults vs. Locals: 'my$$...-default' variables are the persistent
;;    blueprints. Enabling the mode initializes 'my$...-local' variables
;;    from these defaults.
;;
;; 2. Local Buffers: Cycling and scaling target the local variables only.
;;
;; 3. Promotion: 'my>variable-pitch-buffer-face-save-as-default' pushes
;;    local settings back into the persistent global defaults.

;;; Code:

(require 'cl-lib)

;; 1. Variables and Persistence
;; =============================================================================

(defvar my$variable-pitch-buffer-face-list
  '(
    ("Iosevka" 1.15)
    ("Iosevka Aile" 1.15)
    ("Aporetic Sans" 1.15)
    ;; ("SauceCodePro Nerd Font" 1.15)
    ("Cabinet Grotesk" 1.0)
    ("EB Garamond" 1.0)
    ("Trebuchet MS" 1.0)
    ("Inter Variable" 1.0)
    ("PP Fragment" 1.0)
    ("Roboto Flex" 1.0)
    ("Recursive" 1.0)
    ("IBM Plex Sans" 1.0)
    ("Manrope" 1.0)
    ("Alegreya" 1.0)
    ("Source Sans 3" 1.0)
    ("CMU Serif" 1.0)
    ("Lora" 1.0)
    ("ETBembo" 1.0)
    )
  "List of (family normalization-scale) for variable-width cycling.")

(defvar my$variable-pitch-buffer-face-available-list nil
  "Cached list of available fonts from `my$variable-pitch-buffer-face-list`.")

(defcustom my$$variable-pitch-buffer-face-scale-default 1.0
  "The global default zoom factor."
  :type 'float
  :group 'my-variable-pitch)

(defcustom my$$variable-pitch-buffer-face-family-default "Aporetic Sans"
  "The global default font family."
  :type 'string
  :group 'my-variable-pitch)

;; Buffer-local variables for active manipulation
(defvar-local my$variable-pitch-buffer-face-family-local nil)
(defvar-local my$variable-pitch-buffer-face-scale-local nil)


;; 2. Logic and Interactive Commands
;; =============================================================================
(defun my:variable-pitch-buffer-face-ensure-available-list ()
  "Build the available font list on first mode invocation.

The list is built once per Emacs session and contains only fonts that exist
on the current system. For every configured family that does not exist, a
message is written."
  (unless my$variable-pitch-buffer-face-available-list
    (let ((families (font-family-list))
          (available nil)
          (missing nil))
      (dolist (entry my$variable-pitch-buffer-face-list)
        (let ((family (car entry)))
          (if (member family families)
              (push entry available)
            (push family missing))))
      (setq my$variable-pitch-buffer-face-available-list (nreverse available))
      (dolist (family (nreverse missing))
        (message "Variable pitch font not found: %s" family)))))

(defun my>variable-pitch-buffer-face--available-entries ()
  "Return the font entries available for cycling.
Uses the cached available list if present, otherwise falls back to the configured list."
  (or my$variable-pitch-buffer-face-available-list
      my$variable-pitch-buffer-face-list))

(defun my>variable-pitch-buffer-face--current-family ()
  "Return the current family that should be used for cycling and display."
  (or my$variable-pitch-buffer-face-family-local
      my$$variable-pitch-buffer-face-family-default))

(defun my>variable-pitch-buffer-face--cycle-family-entry (backwardp)
  "Return the next or previous font entry.
If BACKWARDP is non-nil, select the previous entry; otherwise select the next entry."
  (let* ((available (my>variable-pitch-buffer-face--available-entries))
         (families (mapcar #'car available))
         (current (my>variable-pitch-buffer-face--current-family))
         (pos (cl-position current families :test #'string=))
         (n (length available)))
    (if (zerop n)
        (car available)
      (setq pos (or pos 0))
      (nth (mod (+ pos (if backwardp -1 1)) n) available))))

(defun my:variable-pitch-buffer-face-apply ()
  "Apply settings using local values, falling back to defaults."
  (if my>variable-pitch-buffer-face-mode
      (let* ((available my$variable-pitch-buffer-face-available-list)
             (family (or my$variable-pitch-buffer-face-family-local
                         my$$variable-pitch-buffer-face-family-default))
             (family-entry (and available (assoc family available)))
             (family (or (car family-entry)
                         (car (car available))
                         family))
             (base-scale (or (and family-entry (cadr family-entry))
                             (and available (cadr (car available)))
                             1.0))
             (user-scale (or my$variable-pitch-buffer-face-scale-local
                             my$$variable-pitch-buffer-face-scale-default))
             (final-scale (* base-scale user-scale)))
        (buffer-face-set `(:family ,family :height ,final-scale)))
    (buffer-face-set nil)))

(defun my>variable-pitch-buffer-face-cycle ()
  "Interactive: Cycle font family for the current buffer ONLY."
  (interactive)
  (setq-local my$variable-pitch-buffer-face-family-local
              (car (my>variable-pitch-buffer-face--cycle-family-entry nil)))
  (my:variable-pitch-buffer-face-apply)
  (message "Buffer Font: %s" my$variable-pitch-buffer-face-family-local))

(defun my>variable-pitch-buffer-face-cycle-backward ()
  "Interactive: Cycle font family backward for the current buffer ONLY."
  (interactive)
  (setq-local my$variable-pitch-buffer-face-family-local
              (car (my>variable-pitch-buffer-face--cycle-family-entry t)))
  (my:variable-pitch-buffer-face-apply)
  (message "Buffer Font: %s" my$variable-pitch-buffer-face-family-local))

(defun my>variable-pitch-buffer-face-current-family-entry ()
  "Return the current font family entry from the font list." 
  (let ((family (or my$variable-pitch-buffer-face-family-local
                    my$$variable-pitch-buffer-face-family-default)))
    (or (assoc family my$variable-pitch-buffer-face-list)
        (assoc family my$variable-pitch-buffer-face-available-list)
        (list family 1.0))))

(defun my:variable-pitch-buffer-face-current-family-base-scale ()
  "Return the current font family's normalization scale." 
  (or (cadr (my>variable-pitch-buffer-face-current-family-entry)) 1.0))

(defun my>variable-pitch-buffer-face-report-current-scales ()
  "Report the current family base scale, local scale, and effective scale."
  (interactive)
  (let* ((family (or my$variable-pitch-buffer-face-family-local
                     my$$variable-pitch-buffer-face-family-default))
         (base-scale (my:variable-pitch-buffer-face-current-family-base-scale))
         (user-scale (or my$variable-pitch-buffer-face-scale-local
                         my$$variable-pitch-buffer-face-scale-default))
         (final-scale (* base-scale user-scale)))
    (message "Font %s: base=%.3f local=%.3f effective=%.3f"
             family base-scale user-scale final-scale)))

(defun my>variable-pitch-buffer-face-adjust-current-family-base-scale (delta)
  "Adjust the current family's normalization scale by DELTA."
  (interactive "nDelta for base scale: ")
  (let* ((family (or my$variable-pitch-buffer-face-family-local
                     my$$variable-pitch-buffer-face-family-default))
         (entry (my>variable-pitch-buffer-face-current-family-entry))
         (new-scale (+ (or (cadr entry) 1.0) delta)))
    (if (assoc family my$variable-pitch-buffer-face-list)
        (setq my$variable-pitch-buffer-face-list
              (mapcar (lambda (item)
                        (if (equal (car item) family)
                            (list family new-scale)
                          item))
                      my$variable-pitch-buffer-face-list))
      (push (list family new-scale) my$variable-pitch-buffer-face-list))
    (setq my$variable-pitch-buffer-face-available-list nil)
    (my:variable-pitch-buffer-face-ensure-available-list)
    (my:variable-pitch-buffer-face-apply)
    (message "Updated %s base scale to %.3f" family new-scale)))

(defun my>variable-pitch-buffer-face-increase-family-base-scale ()
  (interactive)
  (my>variable-pitch-buffer-face-adjust-current-family-base-scale 0.05))

(defun my>variable-pitch-buffer-face-decrease-family-base-scale ()
  (interactive)
  (my>variable-pitch-buffer-face-adjust-current-family-base-scale -0.05))

(defun my:variable-pitch-buffer-face-adjust-scale (delta)
  "Internal: Adjust the local scale."
  (let ((current (or my$variable-pitch-buffer-face-scale-local
                     my$$variable-pitch-buffer-face-scale-default)))
    (setq-local my$variable-pitch-buffer-face-scale-local (+ current delta))
    (my:variable-pitch-buffer-face-apply)
    (message "Buffer Scale: %.2f" my$variable-pitch-buffer-face-scale-local)))

(defun my>variable-pitch-buffer-face-scale-up ()
  (interactive) (my:variable-pitch-buffer-face-adjust-scale 0.05))

(defun my>variable-pitch-buffer-face-scale-down ()
  (interactive) (my:variable-pitch-buffer-face-adjust-scale -0.05))

(defun my>variable-pitch-buffer-face-save-as-default ()
  "Save current buffer's font and scale as the global defaults."
  (interactive)
  (setq my$$variable-pitch-buffer-face-family-default
        (or my$variable-pitch-buffer-face-family-local my$$variable-pitch-buffer-face-family-default))
  (setq my$$variable-pitch-buffer-face-scale-default
        (or my$variable-pitch-buffer-face-scale-local my$$variable-pitch-buffer-face-scale-default))
  (customize-save-variable 'my$$variable-pitch-buffer-face-family-default my$$variable-pitch-buffer-face-family-default)
  (customize-save-variable 'my$$variable-pitch-buffer-face-scale-default my$$variable-pitch-buffer-face-scale-default)
  (message "Saved %s at %.2f as global default."
           my$$variable-pitch-buffer-face-family-default
           my$$variable-pitch-buffer-face-scale-default))


;; 3. The Minor Mode Definition & Keymap
;; =============================================================================

(defvar my>variable-pitch-buffer-face-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c =") 'my>variable-pitch-buffer-face-scale-up)
    (define-key map (kbd "C-c -") 'my>variable-pitch-buffer-face-scale-down)
    ;; (define-key map (kbd "C-c f") 'my>variable-pitch-buffer-face-cycle)
    ;; (define-key map (kbd "C-c b") 'my>variable-pitch-buffer-face-cycle-backward)
    ;; (define-key map (kbd "C-c [") 'my>variable-pitch-buffer-face-decrease-family-base-scale)
    ;; (define-key map (kbd "C-c ]") 'my>variable-pitch-buffer-face-increase-family-base-scale)
    ;; (define-key map (kbd "C-c r") 'my>variable-pitch-buffer-face-report-current-scales)
    ;; (define-key map (kbd "C-c s") 'my>variable-pitch-buffer-face-save-as-default)
    (define-key map (kbd "C-c v") 'my>variable-pitch-buffer-face-show-hydra)
    map)
  "Keymap for `my>variable-pitch-buffer-face-mode'.")

;;;###autoload
(define-minor-mode my>variable-pitch-buffer-face-mode
  "Minor mode to enable normalized variable-pitch font remapping."
  :lighter " VarPitch"
  :keymap my>variable-pitch-buffer-face-mode-map
  :group 'my-variable-pitch
  (if my>variable-pitch-buffer-face-mode
      (progn
        (my:variable-pitch-buffer-face-ensure-available-list)
        ;; Initialize local vars from defaults
        (setq-local my$variable-pitch-buffer-face-family-local my$$variable-pitch-buffer-face-family-default)
        (setq-local my$variable-pitch-buffer-face-scale-local my$$variable-pitch-buffer-face-scale-default)
        (my:variable-pitch-buffer-face-apply))
    (buffer-face-set nil)))


;; 4. Repeat Map Setup
;; =============================================================================

;; (defvar my$variable-pitch-buffer-face-repeat-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "=") 'my>variable-pitch-buffer-face-scale-up)
;;     (define-key map (kbd "+") 'my>variable-pitch-buffer-face-scale-up)
;;     (define-key map (kbd "-") 'my>variable-pitch-buffer-face-scale-down)
;;     (define-key map (kbd "f") 'my>variable-pitch-buffer-face-cycle)
;;     (define-key map (kbd "s") 'my>variable-pitch-buffer-face-save-as-default)
;;     map)
;;   "Repeat map for variable-pitch buffer-face adjustments.")

;; (put 'my>variable-pitch-buffer-face-scale-up   'repeat-map 'my$variable-pitch-buffer-face-repeat-map)
;; (put 'my>variable-pitch-buffer-face-scale-down 'repeat-map 'my$variable-pitch-buffer-face-repeat-map)
;; (put 'my>variable-pitch-buffer-face-cycle      'repeat-map 'my$variable-pitch-buffer-face-repeat-map)


;; 5. Integration
;; =============================================================================

;;(global-set-key (kbd "C-c v") 'my>variable-pitch-buffer-face-mode)

;;(when (fboundp 'repeat-mode) (repeat-mode 1))

;; 6. Hydra
;; =============================================================================

(defun my:variable-pitch-buffer-face-hydra-hint ()
  "Return the current font family, base scale, and local scale for the hydra hint." 
  (let ((family (or my$variable-pitch-buffer-face-family-local
                    my$$variable-pitch-buffer-face-family-default))
        (base-scale (my:variable-pitch-buffer-face-current-family-base-scale))
        (local-scale (or my$variable-pitch-buffer-face-scale-local
                         my$$variable-pitch-buffer-face-scale-default)))
    (format "Font: %s   base=%.3f   local=%.3f"
            family base-scale local-scale)))

;;;###autoload
(defun my>variable-pitch-buffer-face-show-hydra ()
  "Show the variable-pitch buffer-face hydra."
  (interactive)
  (unless (fboundp 'my>variable-pitch-buffer-face-hydra/body)
    (user-error "Hydra support is not available"))
  (my>variable-pitch-buffer-face-hydra/body))

(when (require 'hydra nil t)
  ;;;###autoload
  (defhydra my>variable-pitch-buffer-face-hydra (:hint nil :exit nil :foreign-keys nil
                                                      :pre (my:variable-pitch-buffer-face-ensure-available-list))
    "\nVariable Pitch — %(my:variable-pitch-buffer-face-hydra-hint)\n\n"
    ("t" my>variable-pitch-buffer-face-mode "toggle" :toggle t)
    ("f" my>variable-pitch-buffer-face-cycle "cycle font")
    ("b" my>variable-pitch-buffer-face-cycle-backward "cycle font backwards")
    ("[" my>variable-pitch-buffer-face-decrease-family-base-scale "lower family base")
    ("]" my>variable-pitch-buffer-face-increase-family-base-scale "raise family base")
    ("r" my>variable-pitch-buffer-face-report-current-scales "report scales" :exit t)
    ("=" my>variable-pitch-buffer-face-scale-up "scale up")
    ("-" my>variable-pitch-buffer-face-scale-down "scale down")
    ("s" my>variable-pitch-buffer-face-save-as-default "save default")
    ("q" nil "quit" :exit t)))



(provide 'action/variable-pitch-buffer-face-mode)
;;; variable-pitch-buffer-face-mode.el ends here
