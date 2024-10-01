;;; init-lean.el --- lean configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(zenith/autoload '(lean4-mode) "lean4-mode")
(add-to-list 'auto-mode-alist '("\\.lean\\'" . lean4-mode))

(provide 'init-lean)
;;; init-lean.el ends here
