;;; init-sp.el --- smartparen-mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;; smartparens
(require 'smartparens-config)

(setq sp-autoskip-closing-pair nil) ;; manually add closing pair work as I like

(zenith/add-hook '(comint-mode-hook prog-mode-hook LaTeX-mode-hook org-mode-hook) 'smartparens-mode)

(provide 'init-sp)
;;; init-sp.el ends here
