;;; init-sp.el --- smartparen-mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;; smartparens
(require 'smartparens)
(dolist
    (elt '(comint-mode-hook prog-mode-hook LaTeX-mode-hook org-mode-hook))
  (add-hook elt 'smartparens-mode))

(sp-with-modes '(c-mode c++-mode python-mode) (sp-local-pair "'" "'"))

(provide 'init-sp)
;;; init-sp.el ends here
