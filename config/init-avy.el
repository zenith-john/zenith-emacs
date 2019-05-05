;;; init-avy.el --- redefine evil s key -*- lexical-binding: t; -*-

(use-package avy
  :init
  (setq evil-snipe-local-mode-map nil)
  (general-nvmap "s" 'evil-avy-goto-char))

(provide 'init-avy)
;;; init-avy.el ends here
