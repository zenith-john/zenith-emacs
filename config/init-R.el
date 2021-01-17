;;; init-R. --- ESS configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
(zenith/autoload '(ess-r-mode run-ess-r) "ess-r-mode")
(zenith/autoload '(ess-remote) "essd-els")
(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . ess-r-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]profile\\'" . ess-r-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]history\\'" . ess-r-mode))

(provide 'init-R)
;;; init-R.el ends here
