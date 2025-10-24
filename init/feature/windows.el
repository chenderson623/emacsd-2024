;;; -*- lexical-binding: t; -*-

(use-package ace-window
  :straight t
  :commands (ace-window
             ace-swap-window
             aw-flip-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Use home row for selecting.")
  (aw-scope 'frame "Highlight only current frame."))

(use-package windresize
  :straight t
  :commands windresize)

(use-package buffer-move
  :straight t
  :commands (buf-move-left buf-move-right buf-move-up buf-move-down)
  :custom
  (buffer-move-behavior 'move "move instead of swap"))

;; C-c left (winner-undo), C-c right (winner-redo)
(use-package winner
  :straight (:type built-in)
  :commands (winner-mode winner-undo winner-redo)
  :init (winner-mode)
  :config
  (defvar w/winner-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<left>") 'winner-undo)
      (define-key map (kbd "<right>") 'winner-redo)
      map)
    "Keymap to repeat `winner' key sequences.  Used in `repeat-mode'.")
  (put 'winner-undo 'repeat-map 'w/winner-repeat-map)
  (put 'winner-redo 'repeat-map 'w/winner-repeat-map))

(use-package windmove
  :straight (:type built-in)
  :bind
  (("M-<left>" . windmove-left)
   ("M-<right>" . windmove-right)
   ;;("C-M-<up>" . windmove-up) ;; interferes with org clock adjustment 
   ;;("C-M-<down>" . windmove-down)
   ))

(use-package ace-window
  :straight t
  :bind
  ("<f2>" . ace-window)
  ("M-2" . ace-window)
  :config

  ;; (defvar aw-dispatch-alist
  ;;   '((?x aw-delete-window "Delete Window")
  ;;     (?m aw-swap-window "Swap Windows")
  ;;     (?M aw-move-window "Move Window")
  ;;     (?c aw-copy-window "Copy Window")
  ;;     (?j aw-switch-buffer-in-window "Select Buffer")
  ;;     (?n aw-flip-window)
  ;;     (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
  ;;     (?c aw-split-window-fair "Split Fair Window")
  ;;     (?v aw-split-window-vert "Split Vert Window")
  ;;     (?b aw-split-window-horz "Split Horz Window")
  ;;     (?o delete-other-windows "Delete Other Windows")
  ;;     (?? aw-show-dispatch-help))
  ;;   "List of actions for `aw-dispatch-default'.")

  )

;;
;; Window related
;; https://github.com/abo-abo/hydra/wiki/Window-Management
(defhydra feature-windows>hydra (
                        :exit nil
                        :pre (progn
                               (require 'windmove)
                               (require 'ace-window)
                               (require 'windresize)))
  "window-related"
  ("h" windmove-left "Left" :column "Move")
  ("j" windmove-down "Down" :column "Move")
  ("k" windmove-up "Up" :column "Move")
  ("l" windmove-right "Right" :column "Move")
  ("a" ace-window "Ace" :column "Move" :exit t)
  ;;
  ("o" other-window "Other" :column "Cycle")
  ("n" next-multiframe-window "Next" :column "Cycle")
  ("p" previous-multiframe-window "Prev" :column "Cycle")
  ;;
  ("w" windresize "Resize" :column "Resize" :exit t)
  ("e" enlarge-window-horizontally "<Enlarge>" :column "Resize")
  ("C-e" enlarge-window "Enlarge" :column "Resize")
  ("s" shrink-window-horizontally "<Shrink>" :column "Resize")
  ("C-s" shrink-window "Shrink" :column "Resize")
  ;;
  ("0" delete-window "Close" :column "Split")
  ("1" delete-other-windows "Only 1" :column "Split")
  ("2" split-window-below "/New" :column "Split")
  ("3" split-window-right "|New" :column "Split")
  ("-" split-window-below "/New" :column "Split")
  ("|" split-window-right "|New" :column "Split")
  ;; This is user-defined function.
  ("t" toggle-window-split "Transpose" :column "Split")
  ("S" ace-swap-window "Swap" :column "Split")
  ;;
  ("u" winner-undo "Undo" :column "Winner")
  ("r" winner-redo "Redo" :column "Winner")
  ;;
  ("q" nil "Quit" :column "Misc")
  ("C-g" nil "Quit" :column "Misc"))

(global-set-key (kbd "C-c w") 'feature-windows>hydra/body)


(transient-define-prefix feature-windows>transient-menu ()
  "Most commonly used window commands"
  [["Splits"
    ("s" "Horizontal" split-window-below)
    ("v" "Vertical"   split-window-right)
    ("b" "Balance"    balance-windows)
    ("f" "Fit"        fit-window-to-buffer)
    ("r" "Rotate"     rotate-window-split)
    ]
   ["Window"
    ("c" "Clone Indirect" clone-indirect-buffer)
    ("t" "Tear Off" tear-off-window)
    ("k" "Kill" delete-window)
    ("K" "Kill Buffer+Win"  kill-buffer-and-window)
    ("o" "Kill Others"  delete-other-windows)
    ("m" "Maximize" maximize-window)]
   ["Navigate"
    ("<left>"  "←" windmove-left  :transient t)
    ("<right>" "→" windmove-right :transient t)
    ("<up>"    "↑" windmove-up    :transient t)
    ("<down>"  "↓" windmove-down  :transient t)]
   ["Move"
    ("S-<left>"  "S-←" buf-move-left  :transient t)
    ("S-<right>" "S-→" buf-move-right :transient t)
    ("S-<up>"    "S-↑" buf-move-up    :transient t)
    ("S-<down>"  "S-↓" buf-move-down  :transient t)]
   ["Undo/Redo"
    ("C-<left>" "Winner Undo" winner-undo :transient t)
    ("C-<right>" "Winner Redo" winner-redo :transient t)]])

(defun lem-split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (require 'windmove)
  (split-window-right)
  (windmove-right))

(defun lem-split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (require 'windmove)
  (split-window-below)
  (windmove-down))

(defun toggle-window-split ()
  "Toggle the window splitting style when you have 2 windows."
  (interactive)
  (cond
   ((= (count-windows) 2)
    (let* ((this-win-buffer (window-buffer))
	       (next-win-buffer (window-buffer (next-window)))
	       (this-win-edges (window-edges (selected-window)))
	       (next-win-edges (window-edges (next-window)))
	       (this-win-2nd (not (and (<= (car this-win-edges)
					                   (car next-win-edges))
				                   (<= (cadr this-win-edges)
					                   (cadr next-win-edges)))))
	       (splitter
	        (if (= (car this-win-edges)
		           (car (window-edges (next-window))))
		        'split-window-horizontally
		      'split-window-vertically)))
	  (delete-other-windows)
	  (let ((first-win (selected-window)))
	    (funcall splitter)
	    (if this-win-2nd (other-window 1))
	    (set-window-buffer (selected-window) this-win-buffer)
	    (set-window-buffer (next-window) next-win-buffer)
	    (select-window first-win)
	    (if this-win-2nd (other-window 1)))))
   ;; Give an error if there are more than 2 windows.
   (t
    (message "toggle-window-split only support 2 windows."))))

(provide 'core/windows)
;;; windows.el ends here
