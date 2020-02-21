;;; init-avy.el --- redefine evil s key -*- lexical-binding: t; -*-

;; avy
(use-package avy
  :init
  (setq avy-background t
        avy-all-windows nil)
  (general-define-key
    "M-g s" 'avy-goto-char
    "M-g f" 'avy-goto-char-in-line))

(provide 'init-avy)
;;; init-avy.el ends here
