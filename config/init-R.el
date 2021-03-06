;;; init-R. --- ESS configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
(zenith/autoload '(ess-r-mode run-ess-r) "ess-r-mode")
(zenith/autoload '(ess-remote) "essd-els")
(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . ess-r-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]profile\\'" . ess-r-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]history\\'" . ess-r-mode))

(setq
 ess-R-font-lock-keywords
 '((ess-R-fl-keyword:keywords . t)
   (ess-R-fl-keyword:constants . t)
   (ess-R-fl-keyword:modifiers . t)
   (ess-R-fl-keyword:fun-defs . t)
   (ess-R-fl-keyword:assign-ops . t)
   (ess-R-fl-keyword:%op% . t)
   (ess-fl-keyword:fun-calls . t)
   (ess-fl-keyword:numbers . t)
   (ess-fl-keyword:operators)
   (ess-fl-keyword:delimiters)
   (ess-fl-keyword:=)
   (ess-R-fl-keyword:F&T . t))
 inferior-ess-r-font-lock-keywords
 '((ess-S-fl-keyword:prompt . t)
   (ess-R-fl-keyword:keywords . t)
   (ess-R-fl-keyword:constants . t)
   (ess-R-fl-keyword:modifiers . t)
   (ess-R-fl-keyword:messages . t)
   (ess-R-fl-keyword:fun-defs . t)
   (ess-R-fl-keyword:assign-ops . t)
   (ess-fl-keyword:matrix-labels . t)
   (ess-fl-keyword:fun-calls . t)
   (ess-fl-keyword:numbers . t)
   (ess-fl-keyword:operators)
   (ess-fl-keyword:delimiters)
   (ess-fl-keyword:=)
   (ess-R-fl-keyword:F&T . t)))

(provide 'init-R)
;;; init-R.el ends here
