;;; json.el --- description -*- lexical-binding: t; -*-

;; linter:
;; npm install jsonlint -g

(eval-when-compile
  (require 'mode/prog/treesit)
  (require 'mode/prog/flycheck)
  (require 'pretty-hydra)
  )

(message "Load language/mode")

;;
;;; Init hook for json files
;;
(defun json:json-mode-init()
  (message "HOOK: json:json-mode-init")
  (flycheck-mode +1)
  )
  ;; hook set in filetype-modes.el: (add-hook 'json-ts-mode-hook #'json:json-mode-init)

(unless (executable-find "jsonlint")
      (message "Install jsonlint with `npm install jsonlint -g`"))

(unless (executable-find "prettier")
      (message "Install prettier with `npm install prettier -g`"))

(flycheck-add-mode 'json-jsonlint 'json-mode)
(flycheck-add-mode 'json-jsonlint 'json-ts-mode)

;;
;;; json-mode
;;

;; https://github.com/json-emacs/json-mode
(use-package json-mode
  :straight t
  ;;:mode "\\.json\\'"
  :config
  (message "CONFIG: json-mode")
  )

(use-package json-ts-mode
  :straight (:type built-in)
  ;;:mode ("\\.json\\'" . json-ts-mode)  
  :init
  (unless (treesit-language-available-p 'json)
    (treesit-install-language-grammar 'json))
  ;;(add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
  :config
  (message "USE_PACKAGE:CONFIG: json-ts-mode")
  )

;; https://github.com/DamienCassou/json-navigator
(use-package json-navigator
  :straight t
  :commands json-navigator-navigate-region
  :config
  (straight-use-package 'tree-mode)
  (require 'tree-mode)
  )

;; https://github.com/gongo/json-reformat
(use-package json-reformat
  :straight t
  :commands json-reformat-region)

;; https://github.com/prettier/prettier-emacs
(use-package prettier-js
  :straight t
  :hook
  (json-mode . prettier-js-mode)
  :hook
  (json-ts-mode . prettier-js-mode)
  :config
  (message "USE_PACKAGE:CONFIG: prettier-js")
  )

(pretty-hydra-define json-hydra
                     (:color amaranth :quit-key "q" :title "JSON")
                     (".json"
                      (
                       ("p" #'json-mode-show-path "Copy path to field at point")
                       ("f" #'json-mode-beautify "Format Buffer")
                       ("m"  (lambda () (interactive) (json-pretty-print-buffer t)) "Minify/ugligy buffer")
                       )))

(provide 'mode/language/json)
;;; json.el ends here
