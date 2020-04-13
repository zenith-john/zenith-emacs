;;; init-avy.el --- redefine evil s key -*- lexical-binding: t; -*-

;; avy
(zenith/autoload '(avy-goto-char avy-goto-char-in-line) "avy")
(setq avy-background t
      avy-all-windows nil)

(provide 'init-avy)
;;; init-avy.el ends here
