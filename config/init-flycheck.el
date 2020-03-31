;;; init-flycheck.el --- flycheck configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; flycheck
;; dependencies: pkg-info let-alist seq
(autoload 'flycheck-mode "flycheck" nil t)
(zenith/add-hook
 '(python-mode-hook
   c++-mode-hook
   c-mode-hook) 'flycheck-mode)

;; flycheck-posframe
;; dependencies: flycheck posframe
(with-eval-after-load 'flycheck
  (require 'flycheck-posframe)
  (add-hook 'flycheck-mode-hook 'flycheck-posframe-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
