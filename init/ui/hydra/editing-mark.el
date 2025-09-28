;;; -*- lexical-binding: t; -*-

(defhydra hydra-mark (:body-pre (set-mark-command nil) :color red :hint nil)
    "
_w_, _._: word, symbol                _i'_, _a'_: string  _i)_, _a)_: pair
_t_, _f_: to char (exclude/include)   _0_, _$_: begin/end of line
_;_: comment   _u_: url     _e_: email
_S_: sexp      _d_: defun   _p_: paragraph   _s_: sentence
_h_, _j_, _k_, _l_: move    _x_: xchange mark/point   _q_: quit
  "
    ("t" zop-up-to-char)
    ("f" zop-to-char)
    ("0" move-beginning-of-line)
    ("$" move-end-of-line)
    ("w" er/mark-word)
    ("." er/mark-symbol)
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
    ("x" exchange-point-and-mark)
    ("q" deactivate-mark :exit t)
    )

(provide 'ui/hydra/editing-mark)
