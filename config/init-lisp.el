(use-package sly
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))

(add-hook 'lisp-mode-hook #'+enable-paredit-mode)
(add-hook 'sly-mrepl-mode-hook #'+enable-paredit-mode)

(provide 'init-lisp)
