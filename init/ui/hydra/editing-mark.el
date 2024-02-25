;;; -*- lexical-binding: t; -*-

(message "MY load editing-mark.el")

(defhydra hydra-mark (:body-pre (set-mark-command nil) :color red)
    "
_w_, _,w_: word, symbol    _i'_, _a'_: string  _i)_, _a)_: pair
_t_, _f_: to char (exclude/include)   _0_, _$_: begin/end of line
_;_: comment   _u_: url    _e_: email      _>_: web-mode block or tag
_S_: sexp      _d_: defun  _p_: paragraph  _s_: sentence
_h_, _j_, _k_, _l_: move   _q_: quit
  "
    ("t" mark-to-char-exclusive)
    ("f" mark-to-char-inclusive)
    ("0" move-beginning-of-line)
    ("$" move-end-of-line)
    ("w" er/mark-word)
    (",w" er/mark-symbol)
    ("i'" er/mark-inside-quotes)
    ("a'" er/mark-outside-quotes)
    ("i)" er/mark-inside-pairs)
    ("a)" er/mark-outside-pairs)
    ("i]" er/mark-inside-pairs)
    ("a]" er/mark-outside-pairs)
    ("j" next-line)
    ("k" previous-line)
    ("h" left-char)
    ("l" right-char)
    (";" er/mark-comment)
    ("u" er/mark-url)
    ("e" er/mark-email)
    ("d" er/mark-defun)
    ("S" mark-sexp)
    ("s" mark-end-of-sentence)
    ("p" mark-paragraph)
    (">" web-mode-mark-and-expand)
    ("q" deactivate-mark :exit t)
    )

(provide 'ui/hydra/editing-mark)
