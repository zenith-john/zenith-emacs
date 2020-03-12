;;; init-sp.el --- smartparen-mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;; smartparens
(require 'smartparens)
(zenith/add-hook '(comint-mode-hook prog-mode-hook LaTeX-mode-hook org-mode-hook) 'smartparens-mode)

;; Do not close ' in lisp-mode
(sp-pair "'" nil :actions :rem)
(sp-local-pair sp-c-modes "'" "'")

(provide 'init-sp)
;;; init-sp.el ends here
