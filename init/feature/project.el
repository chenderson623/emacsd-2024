;;; project.el --- project settings -*- lexical-binding: t; -*-

(use-package project
  :straight (:type built-in)
  :config
  (setq project-list-file (emacsd-local-config*filepath "project-list")))

(provide 'feature/project)
