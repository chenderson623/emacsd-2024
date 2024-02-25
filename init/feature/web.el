;;; web.el --- web-related configs -*- lexical-binding: t; -*-

(setq url-cache-directory          (emacs-cache*filepath "url/cache"))
(setq url-configuration-directory  (emacs-state*filepath "url"))
(setq url-cookie-file              (emacs-cache*filepath "url/cache/cookies.el"))
(setq url-history-file             (emacs-state*filepath "url/history.el"))

(provide 'feature/web)
