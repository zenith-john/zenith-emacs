;;; init-sp.el --- smartparen-mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:
(use-package smartparens-config
  :hook ((prog-mode . smartparens-mode)
         (TeX-latex-mode . smartparens-mode)
         (org-mode . smartparens-mode))
  :config
  (general-def emacs-lisp-mode-map "'" nil))

(provide 'init-sp)
;;; init-sp.el ends here
