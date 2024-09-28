
;; https://protesilaos.com/emacs/pulsar
(use-package pulsar
  :defer 5
  :straight (pulsar :host gitlab :repo "protesilaos/pulsar")
  :hook
  (consult-after-jump . pulsar-recenter-top)
  (consult-after-jump . pulsar-reveal-entry)
  ;; integration with the built-in `imenu':
  (imenu-after-jump . pulsar-recenter-top)
  (imenu-after-jump . pulsar-reveal-entry)
  :config
  (pulsar-global-mode 1)
  (setq pulsar-face 'pulsar-magenta
	pulsar-delay 0.05)
  (defun jf/pulse (parg)
    "Pulse the current line.

  If PARG (given as universal prefix), pulse between `point' and `mark'."
    (interactive "P")
    (if (car parg)
	(pulsar--pulse nil nil (point) (mark))
    (pulsar-pulse-line)))
  )
  
