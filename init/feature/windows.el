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
  ;;:bind-keymap (("C-c m w" . windresize-map))
  :commands windresize)

(use-package windmove
  :straight (:type built-in)
  :bind
  (("M-<left>" . windmove-left)
   ("M-<right>" . windmove-right)
   ("C-M-<up>" . windmove-up)
   ("C-M-<down>" . windmove-down)))

;;
;; Window related
;; https://github.com/abo-abo/hydra/wiki/Window-Management
(defhydra hydra-window (;; map key
                        ;; Bind the body
                        ;; mode-specific-map "o"
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

(transient-define-prefix oht-transient-window ()
  "Most commonly used window commands"
  [["Splits"
    ("s" "Horizontal" split-window-below)
    ("v" "Vertical"   split-window-right)
    ("b" "Balance"    balance-windows)
    ("f" "Fit"        fit-window-to-buffer)
    ("r" "Rotate"     oht/rotate-window-split)]
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
    ("s-z" "Winner Undo" winner-undo :transient t)
    ("s-Z" "Winner Redo" winner-redo :transient t)]])

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

(provide 'core/windows)
;;; windows.el ends here
