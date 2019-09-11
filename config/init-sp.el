;;; init-sp.el --- smartparen-mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:
(use-package smartparens-config
  :hook ((prog-mode . smartparens-mode)
         (LaTeX-mode . smartparens-mode)
         (org-mode . smartparens-mode)
         (comint-mode . smartparens-mode))
  :config
  (sp-pair "'" "'" :actions :rem)
  (sp-with-modes '(c-mode c++-mode python-mode) (sp-local-pair "'" "'")))

(provide 'init-sp)
;;; init-sp.el ends here
