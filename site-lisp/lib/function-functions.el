;;; -*- lexical-binding: t; -*-

;; adopted from somewhere
(defun lookup-function (keymap func)
  (let ((all-bindings (where-is-internal (if (symbolp func)
                                             func
                                           (cl-first func))
                                         keymap))
        keys key-bindings)
    (dolist (binding all-bindings)
      (when (and (vectorp binding)
                 (integerp (aref binding 0)))
        (push binding key-bindings)))
    (push (mapconcat #'key-description key-bindings " or ") keys)
    (car keys)))

(provide 'lib/function-functions)
;;; function-functions.el ends here
