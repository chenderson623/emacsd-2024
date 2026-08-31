;;; word-menu.el --- word menu -*- lexical-binding: t; -*-

(use-package engine-mode
  :straight t
  :defer t)

(use-package synosaurus
  :straight t
  :commands (synosaurus-choose-and-replace))

(use-package powerthesaurus
  :straight t
  :commands (powerthesaurus-lookup-word))

(use-package transient
  :straight t
  :defer t)

(defvar my/word-menu-target ""
  "The current target word for word-menu commands.")

(defvar my/word-menu--ready nil
  "Non-nil once engines and transient UI have been defined.")

(defun my/word-menu-region-or-word ()
  "Return the active region (if there is one) or the word at point."
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word t)))

(defun my/word-menu-get-term ()
  "Return the currently set word, or prompt."
  (if (and my/word-menu-target (not (string-empty-p my/word-menu-target)))
      my/word-menu-target
    (let ((selection (my/word-menu-region-or-word)))
      (setq my/word-menu-target
            (if selection
                (read-string (format "Term (%s): " selection) nil nil selection)
              (read-string "Term: " nil nil nil))))))

(defun my>spellcheck-region-or-buffer ()
  "Run spellcheck on the active region (if there is one) or the whole buffer."
  (interactive)
  (if mark-active
      (ispell-region (region-beginning) (region-end))
    (ispell-buffer)))

(defun my>search-powerthesaurus ()
  "Search Powerthesaurus for a prompted term."
  (interactive)
  (powerthesaurus-lookup-word (my/word-menu-get-term)))

(defun my/word-menu-avy-select-word ()
  "Use avy to select a word without moving the point and set it as target."
  (interactive)
  (save-excursion
    (call-interactively 'avy-goto-char-2)
    (setq my/word-menu-target (or (thing-at-point 'word t) "")))
  (my/word-menu-transient))

(defun my/word-menu-ensure-ui ()
  "Load word-menu dependencies and define engines/transient once."
  (unless my/word-menu--ready
    (require 'engine-mode)
    (require 'synosaurus)
    (require 'powerthesaurus)
    (require 'transient)

    (defengine engine-search-wiktionary
               "https://www.wikipedia.org/search-redirect.php?family=wiktionary&search=%s&language=en&go=Go")
    (defengine engine-search-wikipedia
               "http://www.wikipedia.org/search-redirect.php?search=%s&language=en&go=Go")
    (defengine engine-search-etymonline
               "http://etymonline.com/index.php?allowed_in_frame=0&search=%s")
    (defengine engine-search-urban-dictionary
               "http://www.urbandictionary.com/define.php?term=%s")
    (defengine engine-search-thesaurus-com
               "http://www.thesaurus.com/browse/%s")
    (defengine engine-search-old-websters-dictionary
               "https://www.websters1913.com/words/%s")
    (defengine engine-search-merriam-webster-dictionary
               "https://www.merriam-webster.com/dictionary/%s")
    (defengine engine-search-merriam-webster-thesaurus
               "https://www.merriam-webster.com/thesaurus/%s")
    (defengine engine-search-google-translate
               "https://translate.google.com/?sl=auto&tl=en&text=%s&op=translate")

    (defun my>web-search-wiktionary ()
      (interactive)
      (engine/search-engine-search-wiktionary (my/word-menu-get-term)))
    (defun my>web-search-wikipedia ()
      (interactive)
      (engine/search-engine-search-wikipedia (my/word-menu-get-term)))
    (defun my>web-search-etymonline ()
      (interactive)
      (engine/search-engine-search-etymonline (my/word-menu-get-term)))
    (defun my>web-search-urban-dictionary ()
      (interactive)
      (engine/search-engine-search-urban-dictionary (my/word-menu-get-term)))
    (defun my>web-search-thesaurus-com ()
      (interactive)
      (engine/search-engine-search-thesaurus-com (my/word-menu-get-term)))
    (defun my>web-search-old-websters-dictionary ()
      (interactive)
      (engine/search-engine-search-old-websters-dictionary (my/word-menu-get-term)))
    (defun my>web-search-merriam-webster-dictionary ()
      (interactive)
      (engine/search-engine-search-merriam-webster-dictionary (my/word-menu-get-term)))
    (defun my>web-search-merriam-webster-thesaurus ()
      (interactive)
      (engine/search-engine-search-merriam-webster-thesaurus (my/word-menu-get-term)))
    (defun my>web-search-google-translate ()
      (interactive)
      (engine/search-engine-search-google-translate (my/word-menu-get-term)))

    (transient-define-infix my/word-menu-infix-word ()
      "Infix to set the target word."
      :description "Target word"
      :class 'transient-lisp-variable
      :variable 'my/word-menu-target
      :key "-w"
      :reader (lambda (prompt initial-input history)
                (read-string prompt initial-input history)))

    (transient-define-prefix my/word-menu-transient ()
      "Word Menu"
      ["Target"
       ("-w" "Target word" my/word-menu-infix-word)
       ("a" "Avy select word" my/word-menu-avy-select-word)]
      [
       ["Thesauruses"
        ("M" "Merriam-Webster" my>web-search-merriam-webster-thesaurus)
        ("p" "Powerthesaurus" my>search-powerthesaurus)
        ("s" "Synosaurus" synosaurus-choose-and-replace)
        ("t" "Thesaurus.com" my>web-search-thesaurus-com)]
       ["Web Dictionaries"
        ("w m" "Merriam-Webster" my>web-search-merriam-webster-dictionary)
        ("w o" "Webster's 1913" my>web-search-old-websters-dictionary)
        ("w u" "Urban Dictionary" my>web-search-urban-dictionary)
        ("w w" "Wiktionary" my>web-search-wiktionary)
        ("w k" "Wikipedia" my>web-search-wikipedia)]
       ["Tools & Others"
        ("e" "Etymology" my>web-search-etymonline)
        ("r" "Translate" my>web-search-google-translate)
        ("c" "Word count" count-words)
        ("b" "Spellcheck buffer" my>spellcheck-region-or-buffer)
        ("i" "Spellcheck word" ispell-word)]])

    (setq my/word-menu--ready t)))

;;;###autoload
(defun my>word-menu ()
  "Open the Word Menu.
Initializes the target word to the active region or word at point."
  (interactive)
  (my/word-menu-ensure-ui)
  (let ((word (my/word-menu-region-or-word)))
    (setq my/word-menu-target
          (if (and word (not (string-empty-p word)))
              word
            (read-string "Target word: "))))
  (my/word-menu-transient))

(provide 'feature/word-menu)
;;; word-menu.el ends here
